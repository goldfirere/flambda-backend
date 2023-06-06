(** This module handles the logic around the syntax of our extensions to OCaml
    for Jane Street, keeping the gory details wrapped up behind a clean
    interface.

    As we've started to work on syntactic extensions to OCaml, three concerns
    arose about the mechanics of how we wanted to maintain these changes in our
    fork.

    1. We don't want to extend the AST for our fork, as we really want to make
       sure things like ppxen are cross-compatible between upstream and our
       fork.  Thankfully, OCaml already provides places to add extra syntax:
       extension nodes and annotations!  Thus, we have to come up with a way of
       representing our new syntactic constructs in terms of these constructs.

    2. We don't want to actually match on extension nodes or attributes whose
       names are specific strings all over the compiler; that's incredibly
       messy, and it's easy to miss cases, etc.

    3. We want to keep our different novel syntactic features distinct so that
       we can add them to upstream independently, work on them separately, and
       so on.

    We have come up with a design that addresses those concerns by providing
    both a nice compiler-level interface for working with our syntactic
    extensions as first-class AST nodes, as well as a uniform scheme for
    translating this to and from OCaml AST values by using extension nodes or
    attributes.  One wrinkle is that OCaml has many ASTs, one for each syntactic
    category (expressions, patterns, etc.); we have to provide this facility for
    each syntactic category where we want to provide extensions.  A smaller
    wrinkle is that our novel syntactic features come in two varieties:
    *language extensions* (e.g., comprehensions) and *built-in features* (e.g.,
    syntactic function arity).  While the former can be disabled, the latter are
    parse tree changes we rely on (though they won't therefore show up in
    surface syntax).

    a. For each novel syntactic feature, we will define a module (e.g.,
       [Comprehensions]), in which we define a proper AST type per syntactic
       category we care about (e.g., [Comprehensions.expression] and its
       subcomponents).  This addresses concern (3); we've now contained each
       separate feature (and the built-in changes) in a module.  But just doing
       that would leave them too siloed, so…

    b. We define an *overall auxiliary AST* for each syntactic category that's
       just for our novel syntactic features; for expressions, it's called
       [Jane_syntax.Expression.t].  It contains one constructor for each of the
       AST types defined as described in design point (1).  This addresses
       concern (2); we can now match on actual OCaml constructors, as long as we
       can get ahold of them.  And to do that…

    c. We define a general scheme for how we represent our novel syntactic
       features in terms of the existing ASTs, and provide a few primitives for
       consuming/creating AST nodes of this form, for each syntactic category.
       There's not a lot of abstraction to be done, or at least it's not (yet)
       apparent what abstraction there is to do, so most of this remains manual.
       (Setting up a full lens-based/otherwise bidirectional approach sounds
       like a great opportunity for yak-shaving, but not *actually* a good
       idea.)  This solves concern (3), and by doing it uniformly helps us
       address multiple cases at one stroke.

    Then, for each syntactic category, we define a module (in
    [jane_syntax_parsing.ml]) that contains functions for converting between the
    [Parsetree] representation and the higher-level representation. These
    modules are inhabitants of [AST.t], and the [AST] module exposes operations
    on them.

    This module contains the logic for moving to and from OCaml ASTs; the gory
    details of the encoding are detailed in the implementation.  All the actual
    ASTs should live in [Jane_syntax], which is the only module that should
    directly depend on this one.

    When using this module, we often want to specify what our syntax extensions
    look like when desugared into OCaml ASTs, so that we can validate the
    translation code.  We generally specify this as a BNF grammar, but we don't
    want to depend on the specific details of the desugaring.  Thus, instead of
    writing out extension nodes or attributes directly, we write the result of
    [Some_ast.make_extension ~loc [name1; name2; ...; NameN] a] as the special
    syntax [{% 'name1.name2.....nameN' | a %}] in the BNF.  Other pieces of the
    OCaml AST are used as normal.

    One detail which we hide as much as possible is locations: whenever
    constructing an OCaml AST node -- whether with [wrap_desc], the functions in
    [Ast_helper], or some other way -- the location should be left to be
    defaulted (and the default, [!Ast_helper.make_default], should be ghost).
    The [make_entire_jane_syntax] function will handle making sure this default
    location is set appropriately.  If this isn't done and any locations on
    subterms aren't marked as ghost, the compiler will work fine, but ppxlib may
    detect that you've violated its well-formedness constraints and fail to
    parse the resulting AST. *)

(******************************************************************************)

(** The type enumerating our novel syntactic features, which are either a
    language extension (separated out by which one) or the collection of all
    built-in features. *)
module Feature : sig
  type t =
    | Language_extension of Language_extension.Exist.t
    | Builtin
end

(** An AST-style representation of the names used when generating extension
    nodes or attributes for modular syntax.  We use this to abstract over the
    details of how they're encoded, so we have some flexibility in changing them
    (although comments may refer to the specific encoding choices).  This is
    also why we don't expose any functions for rendering or parsing these names;
    that's all handled internally. *)
module Embedded_name : sig
  (** A nonempty list of name components, without the leading root component
      that identifies it as part of the modular syntax mechanism.  This is a
      nonempty list corresponding to the different components of the name: first
      the feature, and then any subparts. *)
  type t = ( :: ) of string * string list

  (** Print out the embedded form of a Jane-syntax name, in quotes; for use in
      error messages. *)
  val pp_quoted_name : Format.formatter -> t -> unit
end

(** An AST desc together with its attributes, including the attributes that
    signal that the AST should be interpreted as a Jane Syntax AST. These are
    produced only by [make_jane_syntax] and [make_entire_jane_syntax].
*)
module With_attributes : sig
  type 'desc t = private
    { jane_syntax_attributes : Parsetree.attributes
    ; desc : 'desc
    }
end

(** Values that lift and lower terms from our novel syntactic
    features from and to an OCaml AST type ([ast]) *)
module AST : sig
  (** One [AST] value per syntactic category we currently care about; we're
      adding these lazily as we need them. When you add another one, make
      sure also to add special handling in [Ast_iterator] and [Ast_mapper].

      ['ast]: The AST type (e.g., [Parsetree.expression])
      ['ast_desc]: The "AST description" type, without the location and
      attributes (e.g., [Parsetree.expression_desc])
  *)
  type ('ast, 'ast_desc) t =
    | Expression :
        (Parsetree.expression, Parsetree.expression_desc With_attributes.t) t
    | Pattern : (Parsetree.pattern, Parsetree.pattern_desc With_attributes.t) t
    | Module_type :
        (Parsetree.module_type, Parsetree.module_type_desc With_attributes.t) t
    | Signature_item :
        (Parsetree.signature_item, Parsetree.signature_item_desc) t
    | Structure_item :
        (Parsetree.structure_item, Parsetree.structure_item_desc) t
    | Core_type :
        (Parsetree.core_type, Parsetree.core_type_desc With_attributes.t) t
    | Constructor_argument :
        (Parsetree.core_type, Parsetree.core_type_desc With_attributes.t) t

  (** Turn an [ast_desc] into an [ast] by adding the appropriate metadata.  When
      creating [ast] nodes afresh to embed our novel syntax, the location should
      be omitted; in this case, it will default to [!Ast_helper.default_loc],
      which should be [ghost]. *)
  val wrap_desc
    :  ('ast, 'ast_desc) t
    -> ?loc:Location.t
    -> attrs:Parsetree.attributes
    -> 'ast_desc
    -> 'ast

  (** Embed a term from one of our novel syntactic features in the AST using the
      given name (the [Embedded_name.t]) and body (the [ast]).  Any locations in
      the generated AST will be set to [!Ast_helper.default_loc], which should
      be [ghost]. *)
  val make_jane_syntax :
    ('ast, 'ast_desc) t -> Embedded_name.t -> 'ast -> 'ast_desc

  (** As [make_jane_syntax], but specifically for the AST node corresponding to
      the entire piece of novel syntax (e.g., for a list comprehension, the
      whole [[x for x in xs]], and not a subcomponent like [for x in xs]).  This
      sets [Ast_helper.default_loc] locally to the [ghost] version of the
      provided location, which is why the [ast] is generated from a function
      call; it is during this call that the location is so set. *)
  val make_entire_jane_syntax
    :  ('ast, 'ast_desc) t
    -> loc:Location.t
    -> string
    -> (unit -> 'ast)
    -> 'ast_desc

  (** Build an [of_ast] function. The return value of this function should be
      used to implement [of_ast] in modules satisfying the signature
      [Jane_syntax.AST].

      The returned function interprets an AST term in the specified syntactic
      category as a term of the appropriate auxiliary extended AST if possible.
      It raises an error if it finds a term from a disabled extension or if the
      embedding is malformed.
  *)
  val make_of_ast :
    ('ast, _) t
    (** Which syntactic category is this for?  E.g., [module AST = Expression].
        ['ast] is the type of novel syntactic terms for this syntactic category,
        across all syntax features. E.g., [Jane_syntax.Expression.t]
    *)
    -> of_ast_internal:(Feature.t -> 'ast -> 'a option)
    (** A function to convert [Parsetree]'s AST to our novel extended one.  The
        choice of feature and the piece of syntax will both be extracted from
        the embedding by the first argument.

        If the given syntax feature does not actually extend the given syntactic
        category, returns [None]; this will be reported as an error. (For
        example: There are no pattern comprehensions, so when building the
        extended pattern AST, this function will return [None] if it spots an
        embedding that claims to be from [Language_extension Comprehensions].)
    *)
    -> ('ast -> 'a option)
end

(** Require that an extension is enabled, or else throw an exception (of an
    abstract type) at the provided location saying otherwise.  This is intended
    to be used in [jane_syntax.ml] when a certain piece of syntax requires two
    extensions to be enabled at once (e.g., immutable array comprehensions such
    as [[:x for x = 1 to 10:]], which require both [Comprehensions] and
    [Immutable_arrays]). *)
val assert_extension_enabled : loc:Location.t -> Language_extension.Exist.t -> unit

(* CR-someday nroberts: An earlier version of this revealed less of its
   implementation in its name: it was called [match_jane_syntax], and
   was a function from ast to ast. This has some advantages (less revealing
   of the Jane Syntax encoding) but I felt it important to document the caller's
   responsibility to plumb through uninterpreted attributes.

   Given that it only has one callsite currently, we decided to keep this
   approach for now, but we could revisit this decision if we use it more
   often.
*)
(** Extracts the first attribute (in list order) that was inserted by the
    Jane Syntax framework, and returns the rest of the attributes in the
    same relative order as was input.

    This can be used by [Jane_syntax] to peel off individual attributes in
    order to process a Jane Syntax element that consists of multiple
    nested ASTs.
*)
val find_and_remove_jane_syntax_attribute :
  Parsetree.attributes -> (Embedded_name.t * Parsetree.attributes) option

(** Errors around the representation of our extended ASTs.  These should mostly
    just be fatal, but they're needed for one test case
    (language-extensions/language_extensions.ml). *)
module Error : sig
  (** An error triggered when desugaring a piece of embedded novel syntax from
      an OCaml AST; left abstract because it should always be fatal *)
  type error

  (** The exception type thrown when desugaring a piece of extended syntax from
      an OCaml AST *)
  exception Error of Location.t * error
end
