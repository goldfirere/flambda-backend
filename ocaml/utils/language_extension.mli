(** Language extensions provided by ocaml-jst *)

(** A setting for extensions that track multiple maturity levels *)
module Maturity : sig
  type t = Stable | Beta | Alpha
end

(** The type of language extensions. An ['a t] is an extension that can either
    be off or be set to have any value in ['a], so a [unit t] can be either on
    or off, while a [maturity t] can have different maturity settings. *)
type _ t =
  | Comprehensions : unit t
  | Local : unit t
  | Include_functor : unit t
  | Polymorphic_parameters : unit t
  | Immutable_arrays : unit t
  | Module_strengthening : unit t
  | Layouts : Maturity.t t

(** Existentially packed language extension *)
module Exist : sig
  include module type of Misc.Exist(struct type nonrec 'a t = 'a t end)

  val to_string : t -> string
end

(** Equality on language extensions *)
val equal : 'a t -> 'b t -> bool

(** Disable all extensions *)
val disable_all : unit -> unit

(** Maximally enable all extensions (that is, set to [Alpha] for [maturity]
    extensions. *)
val enable_maximal : unit -> unit

(** Check if a language extension is "erasable", i.e. whether it can be
    harmlessly translated to attributes and compiled with the upstream
    compiler. *)
val is_erasable : 'a t -> bool

(** Print and parse language extensions; parsing is case-insensitive *)
val to_string : 'a t -> string
val of_string : string -> Exist.t option
val of_string_exn : string -> Exist.t

(** Enable and disable language extensions; these operations are idempotent *)
val set : 'a t -> 'a option -> unit
val enable : 'a t -> 'a -> unit
val disable : 'a t -> unit

(** Check if a language extension is currently enabled *)
val is_enabled : unit t -> bool

(** Check if a language extension is enabled at least at the given level *)
val is_at_least : 'a t -> 'a -> bool

(** Tooling support: Temporarily enable and disable language extensions; these
    operations are idempotent.  Calls to [set], [enable], [disable], and
    [disallow_extensions] inside the body of the function argument will also
    be rolled back when the function finishes, but this behavior may change;
    nest multiple [with_*] functions instead.  *)
val with_set : 'a t -> 'a option -> (unit -> unit) -> unit
val with_enabled : 'a t -> 'a -> (unit -> unit) -> unit
val with_disabled : 'a t -> (unit -> unit) -> unit

(** Permanently restrict the allowable extensions to those that are
    "erasable", i.e. those that can be harmlessly translated to attributes and
    compiled with the upstream compiler.  Used for [-only-erasable-extensions]
    to ensure that some code is guaranteed to be compatible with upstream
    OCaml after rewriting to attributes.  When called, disables any
    currently-enabled non-erasable extensions, including any that are on by
    default.  Causes any future uses of [set ~enabled:true], [enable], and
    their [with_] variants to raise if used with a non-erasable extension.
    The [is_enabled] function will still work on any extensions, it will just
    always return [false] on non-erasable ones.  Will raise if called after
    [disallow_extensions]; the ratchet of extension restriction only goes one
    way. *)
val restrict_to_erasable_extensions : unit -> unit

(** Permanently ban all extensions; used for [-disable-all-extensions] to
    ensure that some code is 100% extension-free.  When called, disables any
    currently-enabled extensions, including the defaults.  Causes any future
    uses of [set ~enabled:true], [enable], and their [with_] variants to
    raise; also causes any future uses of [only_erasable_extensions] to raise.
    The [is_enabled] function will still work, it will just always return
    [false].*)
val disallow_extensions : unit -> unit
