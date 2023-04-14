(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Chris Casinghino, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Layouts *)

module Sort = struct
  type const =
    | Void
    | Value

  type t =
    | Var of var
    | Const of const
  and var = t option ref

  let void = Const Void
  let value = Const Value

  let of_const = function
    | Void -> void
    | Value -> value

  let of_var v = Var v

  let new_var () = Var (ref None)

  let rec repr ~default : t -> t = function
    | Const _ as t -> t
    | Var r as t -> begin match !r with
      | None -> begin match default with
        | None -> t
        | Some const -> begin
            let t = of_const const in
            r := Some t;
            t
          end
      end
      | Some s -> begin
          let result = repr ~default s in
          r := Some result; (* path compression *)
          result
        end
    end

  (***********************)
  (* equality *)

  type equate_result =
    | Unequal
    | Equal_mutated_first
    | Equal_mutated_second
    | Equal_no_mutation

  let swap_equate_result = function
    | Equal_mutated_first -> Equal_mutated_second
    | Equal_mutated_second -> Equal_mutated_first
    | (Unequal | Equal_no_mutation) as r -> r

  let equal_const_const c1 c2 = match c1, c2 with
    | Void, Void
    | Value, Value -> Equal_no_mutation
    | Void, Value
    | Value, Void -> Unequal

  let rec equate_var_const v1 c2 = match !v1 with
    | Some s1 -> equate_sort_const s1 c2
    | None -> v1 := Some (of_const c2); Equal_mutated_first

  and equate_var v1 s2 = match s2 with
    | Const c2 -> equate_var_const v1 c2
    | Var v2 -> equate_var_var v1 v2

  and equate_var_var v1 v2 =
    if v1 == v2 then
      Equal_no_mutation
    else begin
      match !v1, !v2 with
      | Some s1, _ -> swap_equate_result (equate_var v2 s1)
      | _, Some s2 -> equate_var v1 s2
      | None, None -> v1 := Some (of_var v2); Equal_mutated_first
    end

  and equate_sort_const s1 c2 = match s1 with
    | Const c1 -> equal_const_const c1 c2
    | Var v1 -> equate_var_const v1 c2

  let equate_tracking_mutation s1 s2 = match s1 with
    | Const c1 -> swap_equate_result (equate_sort_const s2 c1)
    | Var v1 -> equate_var v1 s2

  (* Don't expose whether or not mutation happened; we just need that for [Layout] *)
  let equate s1 s2 = match equate_tracking_mutation s1 s2 with
    | Unequal -> false
    | Equal_mutated_first | Equal_mutated_second | Equal_no_mutation -> true

  let to_string = function
    | Var _ -> "<sort variable>"
    | Const Value -> "value"
    | Const Void -> "void"
  let format ppf t = Format.fprintf ppf "%s" (to_string t)

  (*** defaulting ***)

  let get_defaulting ~default t =
    match repr ~default:(Some default) t with
    | Const result -> result
    | Var _ -> assert false

  let constrain_default_void = get_defaulting ~default:Void
  let can_make_void t = Void = constrain_default_void t

  let default_to_value t =
    ignore (get_defaulting ~default:Value t)

  module Debug_printers = struct
    open Format

    let rec t ppf = function
      | Var v   -> fprintf ppf "Var %a" var v
      | Const c -> fprintf ppf (match c with
                                | Void  -> "Void"
                                | Value -> "Value")

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None   -> fprintf ppf "None"

    and var ppf v = fprintf ppf "{ contents = %a }" opt_t (!v)
  end
end

type sort = Sort.t

module Layout = struct
  type concrete_layout_reason =
    | Match
    | Constructor_declaration of int
    | Label_declaration of Ident.t
    | Unannotated_type_parameter
    | Record_projection
    | Record_assignment
    | Let_binding
    | Structure_element

  type annotation_context =
    | Type_declaration of Path.t
    | Type_parameter of Path.t * string
    | With_constraint of string
    | Newtype_declaration of string

  type intersection_reason =
    | Gadt_equation of Path.t
    | Tyvar_refinement
    (* CR layouts: this needs to carry a type_expr, but that's loopy *)

  type value_creation_reason =
    | Class_let_binding
    | Function_argument
    | Function_result
    | Tuple_element
    | Probe
    | Package_hack
    | Object
    | Instance_variable
    | Object_field
    | Class_field
    | Boxed_record
    | Boxed_variant
    | Extensible_variant
    | Primitive
    | Type_argument
    | Tuple
    | Row_variable
    | Polymorphic_variant
    | Arrow
    | Tfield
    | Tnil
    | First_class_module
    | Separability_check
    | Univar
    | Polymorphic_variant_field
    | Default_type_layout
    | Float_record_field
    | Existential_type_variable
    | Array_element
    | Lazy_expression
    | Class_argument
    | Structure_element
    | Unknown of string

  type immediate_creation_reason =
    | Empty_record
    | Empty_variant
    | Primitive
    | Immediate_polymorphic_variant
    | Gc_ignorable_check
    | Value_kind

  type immediate64_creation_reason =
    | Local_mode_cross_check
    | Gc_ignorable_check
    | Separability_check

  type void_creation_reason =
    | Sanity_check

  type any_creation_reason =
    | Missing_cmi
    | Wildcard
    | Unification_var
    | Initial_typedecl_env
    | Dummy_layout

  type creation_reason =
    | Annotated of annotation_context * Location.t
    | Value_creation of value_creation_reason
    | Immediate_creation of immediate_creation_reason
    | Immediate64_creation of immediate64_creation_reason
    | Void_creation of void_creation_reason
    | Any_creation of any_creation_reason
    | Concrete_creation of concrete_layout_reason

  type history =
    | Intersection of intersection_reason * history * history
    | Sublayout of history * history
    | Creation of creation_reason

  type internal =
    | Any
    | Sort of sort
    | Immediate64
    (** We know for sure that values of types of this layout are always immediate
        on 64-bit platforms. For other platforms, we know nothing about immediacy.
    *)
    | Immediate

  type t =
    { layout : internal
    ; history : history }

  let fresh_layout layout ~creation = { layout; history = Creation creation }

  (******************************)
  (* constants *)

  let any ~creation = fresh_layout Any ~creation:(Any_creation creation)
  let void ~creation =
    fresh_layout (Sort Sort.void) ~creation:(Void_creation creation)
  let value ~creation =
    fresh_layout (Sort Sort.value) ~creation:(Value_creation creation)
  let immediate64 ~creation =
    fresh_layout Immediate64 ~creation:(Immediate64_creation creation)
  let immediate ~creation =
    fresh_layout Immediate ~creation:(Immediate_creation creation)

  type const = Asttypes.const_layout =
    | Any
    | Value
    | Void
    | Immediate64
    | Immediate

  let string_of_const : const -> _ = function
    | Any -> "any"
    | Value -> "value"
    | Void -> "void"
    | Immediate64 -> "immediate64"
    | Immediate -> "immediate"

  let equal_const (c1 : const) (c2 : const) = match c1, c2 with
    | Any, Any -> true
    | Immediate64, Immediate64 -> true
    | Immediate, Immediate -> true
    | Void, Void -> true
    | Value, Value -> true
    | (Any | Immediate64 | Immediate | Void | Value), _ -> false

 (******************************)
  (* construction *)

  let of_new_sort_var ~creation =
    fresh_layout (Sort (Sort.new_var ())) ~creation:(Concrete_creation creation)

  let of_sort ~creation s =
    fresh_layout (Sort s) ~creation:(Concrete_creation creation)

  let of_const ~creation : const -> t = function
    | Any -> fresh_layout Any ~creation
    | Immediate -> fresh_layout Immediate ~creation
    | Immediate64 -> fresh_layout Immediate64 ~creation
    | Value -> fresh_layout (Sort Sort.value) ~creation
    | Void -> fresh_layout (Sort Sort.void) ~creation

  let of_attributes ~legacy_immediate ~reason attrs =
    match Builtin_attributes.layout ~legacy_immediate attrs with
    | Ok None as a -> a
    | Ok (Some l) -> Ok (Some (of_const ~creation:(Annotated (reason, l.loc))
                                 l.txt))
    | Error _ as e -> e

  let of_attributes_default ~legacy_immediate ~reason ~default attrs =
    match of_attributes ~legacy_immediate ~reason attrs with
    | Ok None -> Ok default
    | Ok (Some l) -> Ok l
    | Error _ as e -> e

  (******************************)
  (* elimination *)

  type desc =
    | Const of const
    | Var of Sort.var

  let repr ~default (t : t) : desc = match t.layout with
    | Any -> Const Any
    | Immediate -> Const Immediate
    | Immediate64 -> Const Immediate64
    | Sort s -> begin match Sort.repr ~default s with
      (* NB: this match isn't as silly as it looks: those are
         different constructors on the left than on the right *)
      | Const Void -> Const Void
      | Const Value -> Const Value
      | Var v -> Var v
    end

  let get = repr ~default:None

  (* CR layouts: this function is suspect; it seems likely to reisenberg
     that refactoring could get rid of it *)
  let sort_of_layout l =
    match get l with
    | Const Void -> Sort.void
    | Const (Value | Immediate | Immediate64) -> Sort.value
    | Const Any -> Misc.fatal_error "Layout.sort_of_layout"
    | Var v -> Sort.of_var v

  (*********************************)
  (* pretty printing *)

  let to_string lay = match get lay with
    | Const c -> string_of_const c
    | Var _ -> "<sort variable>"

  module Formatting : sig
    open Format
    val format : formatter -> t -> unit
    val format_history :
      pp_name:(formatter -> 'a -> unit) -> name:'a ->
      formatter -> t -> unit
  end = struct
    open Format

    let format ppf t = fprintf ppf "%s" (to_string t)

    let format_concrete_layout_reason ppf : concrete_layout_reason -> unit =
      function
      | Match ->
        fprintf ppf "matched on"
      | Constructor_declaration idx ->
        fprintf ppf "used as constructor field %d" idx
      | Label_declaration lbl ->
        fprintf ppf "used in the declaration of the record field \"%a\""
          Ident.print lbl
      | Unannotated_type_parameter ->
        fprintf ppf "appears as an unannotated type parameter"
      | Record_projection ->
        fprintf ppf "used as the record in a projection"
      | Record_assignment ->
        fprintf ppf "used as the record in an assignment"
      | Let_binding ->
        fprintf ppf "bound by a `let`"
      | Structure_element ->
        fprintf ppf "stored in a module structure"

    let format_annotation_context ppf : annotation_context -> unit = function
      | Type_declaration p ->
          fprintf ppf "the declaration of the type %a"
            Path.print p
      | Type_parameter (p, var) ->
          fprintf ppf "%s@ in the declaration of the type %a"
            var
            Path.print p
      | With_constraint s ->
          fprintf ppf "the `with` constraint for %s" s
      | Newtype_declaration name ->
          fprintf ppf "the abstract type declaration for %s"
            name

    let format_any_creation_reason ppf = function
      | Missing_cmi ->
         fprintf ppf "XXX layouts"
      | Wildcard ->
         fprintf ppf "a _ in a type"
      | Unification_var ->
         fprintf ppf "a fresh unification variable"
      | Initial_typedecl_env ->
         fprintf ppf "a dummy layout used in checking mutually recursive datatypes"
           (* XXX layouts: fix that output *)
      | Dummy_layout ->
         fprintf ppf "@[a dummy layout that should have been overwritten;@ \
                      Please notify the Jane Street compilers group if you see this output."
    (* CR layouts: Improve output or remove this constructor *)

    let format_immediate_creation_reason ppf : immediate_creation_reason -> _ =
      function
      | Empty_record ->
         fprintf ppf "a record containing all void elements"
      | Empty_variant ->
         fprintf ppf "a variant containing all void elements"
      | Primitive ->
         fprintf ppf "a primitive immediate type"
      | Immediate_polymorphic_variant ->
         fprintf ppf "an immediate polymorphic variant"
      | Gc_ignorable_check ->
         fprintf ppf "the check to see whether a value can be ignored by GC"
      | Value_kind ->
         fprintf ppf
           "the check to see whether a polymorphic variant is immediate"

    let format_immediate64_creation_reason ppf = function
      | Local_mode_cross_check ->
         fprintf ppf "the check for whether a local value can safely escape"
      | Gc_ignorable_check ->
         fprintf ppf "the check to see whether a value can be ignored by GC"
      | Separability_check ->
         fprintf ppf "the check that a type is definitely not `float`"

    let format_value_creation_reason ppf : value_creation_reason -> _ = function
      | Class_let_binding -> fprintf ppf "let-bound in a class expression"
      | Function_argument -> fprintf ppf "a function argument"
      | Function_result -> fprintf ppf "a function result"
      | Tuple_element -> fprintf ppf "a tuple element"
      | Probe -> fprintf ppf "a probe"
      | Package_hack -> fprintf ppf "involved in the package hack"
      (* XXX layouts: figure out what "package hack" means *)
      | Object -> fprintf ppf "an object"
      | Instance_variable -> fprintf ppf "an instance variable"
      | Object_field -> fprintf ppf "an object field"
      | Class_field -> fprintf ppf "an class field"
      | Boxed_record -> fprintf ppf "a boxed record"
      | Boxed_variant -> fprintf ppf "a boxed variant"
      | Extensible_variant -> fprintf ppf "an extensible variant"
      | Primitive -> fprintf ppf "a primitive value type"
      | Type_argument -> fprintf ppf "a type argument defaulted to have layout value"
      | Tuple -> fprintf ppf "a tuple type"
      | Row_variable -> fprintf ppf "a row variable"
      | Polymorphic_variant -> fprintf ppf "a polymorphic variant"
      | Arrow -> fprintf ppf "a function type"
      | Tfield -> fprintf ppf "an internal Tfield type (you shouldn't see this)"
      | Tnil -> fprintf ppf "an internal Tnil type (you shouldn't see this)"
      | First_class_module -> fprintf ppf "a first-class module type"
      | Separability_check ->
        fprintf ppf "the check that a type is definitely not `float`"
      | Univar -> fprintf ppf "an unannotated universal variable"
      | Polymorphic_variant_field -> fprintf ppf "a field of a polymorphic variant"
      | Default_type_layout ->
        fprintf ppf "the default layout for an abstract type"
      | Float_record_field ->
        fprintf ppf "a field of a float record"
      | Existential_type_variable ->
        fprintf ppf "an unannotated existential type variable"
      | Array_element ->
        fprintf ppf "an array element"
      | Lazy_expression ->
        fprintf ppf "a lazy expression"
      | Class_argument ->
        fprintf ppf "a term-level argument to a class constructor"
      | Structure_element ->
        fprintf ppf "stored in a module structure"
      | Unknown s -> fprintf ppf "unknown @[(please alert the Jane Street@;\
                       compilers team with this message: %s)@]" s


    let format_void_creation_reason ppf : void_creation_reason -> _ = function
      | Sanity_check -> fprintf ppf "check to make sure there are no voids"
        (* CR layouts: remove this when we remove its uses *)

    let format_creation_reason ppf : creation_reason -> unit = function
      | Annotated (ctx, _) ->
          fprintf ppf "annotation %a" format_annotation_context ctx
      | Any_creation any ->
         format_any_creation_reason ppf any
      | Immediate_creation immediate ->
         format_immediate_creation_reason ppf immediate
      | Immediate64_creation immediate64 ->
         format_immediate64_creation_reason ppf immediate64
      | Void_creation void ->
        format_void_creation_reason ppf void
      | Value_creation value ->
         format_value_creation_reason ppf value
      | Concrete_creation concrete ->
         format_concrete_layout_reason ppf concrete

    let format_intersection_reason ppf = function
      | Gadt_equation name ->
        fprintf ppf "a GADT match on the constructor %a" Path.print name
      | Tyvar_refinement ->
        fprintf ppf "updating a type variable"

    let format_history ~pp_name ~name ppf t =
      (* CR layouts: Restore printing *)
      if false then begin
        (* CR layouts: Improve this output *)
        let rec in_order ppf = function
          | Intersection (reason, h1, h2) ->
            fprintf ppf "@[<v 2>%a@]@;intersection (%a)@;@[<v 2>%a@]"
              in_order h1
              format_intersection_reason reason
              in_order h2
          | Sublayout (h1, h2) ->
            fprintf ppf "@[<v 2>%a@]@;sublayout@;@[<v 2>%a@]"
              in_order h1
              in_order h2
          | Creation c ->
            format_creation_reason ppf c
        in
        fprintf ppf "%a has this layout history:@;@[<v 2>%a@]"
          pp_name name
          in_order t.history
      end;
      let rec check_missing_cmi = function
        | Intersection (_, h1, h2) | Sublayout (h1, h2) ->
          check_missing_cmi h1 || check_missing_cmi h2
        | Creation (Any_creation Missing_cmi) -> true
        | _ -> false
      in
      if check_missing_cmi t.history then
        fprintf ppf "missing cmi"
  end

  include Formatting

  (******************************)
  (* errors *)

  module Violation = struct
    open Format

    type nonrec t =
      | Not_a_sublayout of t * t
      | No_intersection of t * t

    let report_general preamble pp_former former ppf t =
      let l1, problem, l2 = match t with
        | Not_a_sublayout(l1, l2) -> l1, "is not a sublayout of", l2
        | No_intersection(l1, l2) -> l1, "does not overlap with", l2
      in
      fprintf ppf "@[<v>@[<hov 2>%s%a has layout %a,@ which %s %a.@]%a%a@]"
        preamble
        pp_former former
        format l1
        problem
        format l2
        (format_history ~pp_name:pp_former ~name:former) l1
        (format_history ~pp_name:pp_print_string ~name:"The latter") l2

    let pp_t ppf x = fprintf ppf "%t" x

    let report_with_offender ~offender =
      report_general "" pp_t offender

    let report_with_offender_sort ~offender =
      report_general "A representable layout was expected, but " pp_t offender

    let report_with_name ~name =
      report_general "" pp_print_string name
  end

  (******************************)
  (* relations *)

  let equate_or_equal ~allow_mutation (l1 : t) (l2 : t) =
    match l1.layout, l2.layout with
    | Any, Any -> true
    | Immediate64, Immediate64 -> true
    | Immediate, Immediate -> true
    | Sort s1, Sort s2 -> begin
        match Sort.equate_tracking_mutation s1 s2 with
        | (Equal_mutated_first | Equal_mutated_second)
          when not allow_mutation ->
            Misc.fatal_errorf
              "Layouts.Layout.equal: Performed unexpected mutation"
        | Unequal -> false
        | Equal_no_mutation | Equal_mutated_first | Equal_mutated_second -> true
      end
    | (Any | Immediate64 | Immediate | Sort _), _ -> false

  (* XXX ASZ: Switch this back to ~allow_mutation:false *)
  let equal = equate_or_equal ~allow_mutation:true

  let equate = equate_or_equal ~allow_mutation:true

  let intersection ~reason l1 l2 =
    let err = Error (Violation.No_intersection (l1, l2)) in
    let equality_check is_eq l = if is_eq then Ok l else err in
    (* it's OK not to cache the result of [get], because [get] does path
       compression *)
    let intersection_const c1 c2 = match c1, c2 with
      | c, Any | Any, c -> Ok c
      | c1, c2 when equal_const c1 c2 -> Ok c1
      | Immediate, (Immediate64 | Value)
      | (Immediate64 | Value), Immediate -> Ok Immediate
      | Immediate64, Value | Value, Immediate64 -> Ok Immediate64
      | (Value | Void | Immediate64 | Immediate), _ -> err
    in
    let internal_of_const : const -> internal = function
      | Any -> Any
      | Value -> Sort Sort.value
      | Void -> Sort Sort.void
      | Immediate -> Immediate
      | Immediate64 -> Immediate64
    in
    let combine_histories l =
      { l with history = Intersection (reason, l1.history, l2.history) }
    in
    Result.map combine_histories @@ match get l1, get l2 with
    | Const c1, Const c2 ->
      Result.map (fun c -> { l1 with layout = internal_of_const c })
                 (intersection_const c1 c2)
    | Var _, Const Any -> Ok l1
    | Const Any, Var _ -> Ok { l1 with layout = l2.layout }
    | Var v, Const ((Immediate64 | Immediate) as imm)
    | Const ((Immediate64 | Immediate) as imm), Var v ->
        equality_check (Sort.equate (Sort.of_var v) Sort.value)
          { l1 with layout = internal_of_const imm }
    | _, _ -> equality_check (equate l1 l2) l1

  let sub sub super =
    let ok = Ok sub in
    let err = Error (Violation.Not_a_sublayout (sub,super)) in
    let check good = if good then ok else err in
    let sub_const c1 c2 = match c1, c2 with
      | _, Any -> true
      | c1, c2 when equal_const c1 c2 -> true
      | (Immediate | Immediate64), Value -> true
      | Immediate, Immediate64 -> true
      | (Any | Void | Value | Immediate64 | Immediate), _ -> false
    in
    let combine_histories l =
      { l with history = Sublayout (sub.history, super.history) }
    in
    Result.map combine_histories @@ match get sub, get super with
    | Const c1, Const c2 -> check (sub_const c1 c2)
    | Var v1, Var v2 ->
      check (Sort.equate (Sort.of_var v1) (Sort.of_var v2))
    | Var _, Const Any -> ok
    | Var v, Const Value
    | Const (Immediate64 | Immediate | Value), Var v ->
      check (Sort.equate (Sort.of_var v) Sort.value)
    | Var v, Const Void
    | Const Void, Var v ->
      check (Sort.equate (Sort.of_var v) Sort.void)
    | Const Any, Var _ -> err
    | Var _, Const (Immediate | Immediate64) -> err

  let is_void = function
    | { layout = Sort (Const Void) } -> true
    | { layout = Sort (Var _) } -> assert false
    | _ -> false

  (*********************************)
  (* defaulting *)

  let get_defaulting ~default t =
    match repr ~default:(Some default) t with
    | Const result -> result
    | Var _ -> assert false

  let constrain_default_void = get_defaulting ~default:Sort.Void
  let can_make_void l = Void = constrain_default_void l
  let default_to_value t =
    ignore (get_defaulting ~default:Value t)

  (*********************************)
  (* debugging *)

  module Debug_printers = struct
    open Format

    let internal ppf : internal -> unit = function
      | Any         -> fprintf ppf "Any"
      | Sort s      -> fprintf ppf "Sort %a" Sort.Debug_printers.t s
      | Immediate64 -> fprintf ppf "Immediate64"
      | Immediate   -> fprintf ppf "Immediate"

    let concrete_layout_reason ppf : concrete_layout_reason -> unit = function
      | Match ->
          fprintf ppf "Match"
      | Constructor_declaration idx ->
          fprintf ppf "Constructor_declaration %d" idx
      | Label_declaration lbl ->
          fprintf ppf "Label_declaration %a" Ident.print lbl
      | Unannotated_type_parameter ->
          fprintf ppf "Unannotated_type_parameter"
      | Record_projection ->
          fprintf ppf "Record_projection"
      | Record_assignment ->
          fprintf ppf "Record_assignment"
      | Let_binding ->
          fprintf ppf "Let_binding"
      | Structure_element ->
          fprintf ppf "Structure_element"

    let annotation_context ppf : annotation_context -> unit = function
      | Type_declaration p ->
          fprintf ppf "Type_declaration %a" Path.print p
      | Type_parameter (p, var) ->
          fprintf ppf "Type_parameter (%a, %S)" Path.print p var
      | With_constraint s ->
          fprintf ppf "With_constraint %s" s
      | Newtype_declaration name ->
          fprintf ppf "Newtype_declaration %s" name

    let any_creation_reason ppf = function
      | Missing_cmi -> fprintf ppf "Missing_cmi"
      | Wildcard -> fprintf ppf "Wildcard"
      | Unification_var -> fprintf ppf "Unification_var"
      | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
      | Dummy_layout -> fprintf ppf "Dummy_layout"

    let immediate_creation_reason ppf : immediate_creation_reason -> _ =
      function
      | Empty_record -> fprintf ppf "Empty_record"
      | Empty_variant -> fprintf ppf "Empty_variant"
      | Primitive -> fprintf ppf "Primitive"
      | Immediate_polymorphic_variant ->
        fprintf ppf "Immediate_polymorphic_variant"
      | Gc_ignorable_check -> fprintf ppf "Gc_ignorable_check"
      | Value_kind -> fprintf ppf "Value_kind"

    let immediate64_creation_reason ppf = function
      | Local_mode_cross_check -> fprintf ppf "Local_mode_cross_check"
      | Gc_ignorable_check -> fprintf ppf "Gc_ignorable_check"
      | Separability_check -> fprintf ppf "Separability_check"

    let value_creation_reason ppf : value_creation_reason -> _ = function
      | Class_let_binding -> fprintf ppf "Class_let_binding"
      | Function_argument -> fprintf ppf "Function_argument"
      | Function_result -> fprintf ppf "Function_result"
      | Tuple_element -> fprintf ppf "Tuple_element"
      | Probe -> fprintf ppf "Probe"
      | Package_hack -> fprintf ppf "Package_hack"
      | Object -> fprintf ppf "Object"
      | Instance_variable -> fprintf ppf "Instance_variable"
      | Object_field -> fprintf ppf "Object_field"
      | Class_field -> fprintf ppf "Class_field"
      | Boxed_record -> fprintf ppf "Boxed_record"
      | Boxed_variant -> fprintf ppf "Boxed_variant"
      | Extensible_variant -> fprintf ppf "Extensible_variant"
      | Primitive -> fprintf ppf "Primitive"
      | Type_argument -> fprintf ppf "Type_argument"
      | Tuple -> fprintf ppf "Tuple"
      | Row_variable -> fprintf ppf "Row_variable"
      | Polymorphic_variant -> fprintf ppf "Polymorphic_variant"
      | Arrow -> fprintf ppf "Arrow"
      | Tfield -> fprintf ppf "Tfield"
      | Tnil -> fprintf ppf "Tnil"
      | First_class_module -> fprintf ppf "First_class_module"
      | Separability_check -> fprintf ppf "Separability_check"
      | Univar -> fprintf ppf "Univar"
      | Polymorphic_variant_field -> fprintf ppf "Polymorphic_variant_field"
      | Default_type_layout -> fprintf ppf "Default_type_layout"
      | Float_record_field -> fprintf ppf "Float_record_field"
      | Existential_type_variable -> fprintf ppf "Existential_type_variable"
      | Array_element -> fprintf ppf "Array_element"
      | Lazy_expression -> fprintf ppf "Lazy_expression"
      | Class_argument -> fprintf ppf "Class_argument"
      | Structure_element -> fprintf ppf "Structure_element"
      | Unknown s -> fprintf ppf "Unknown %s" s

    let void_creation_reason ppf : void_creation_reason -> _ = function
      | Sanity_check -> fprintf ppf "Sanity_check"

    let creation_reason ppf : creation_reason -> unit = function
      | Annotated (ctx, loc) ->
        fprintf ppf "Annotated (%a,%a)"
          annotation_context ctx
          Location.print_loc loc
      | Any_creation any ->
         fprintf ppf "Any_creation %a" any_creation_reason any
      | Immediate_creation immediate ->
         fprintf ppf "Immediate_creation %a" immediate_creation_reason immediate
      | Immediate64_creation immediate64 ->
         fprintf ppf "Immediate64_creation %a" immediate64_creation_reason immediate64
      | Value_creation value ->
         fprintf ppf "Value_creation %a" value_creation_reason value
      | Void_creation void ->
         fprintf ppf "Void_creation %a" void_creation_reason void
      | Concrete_creation concrete ->
         fprintf ppf "Concrete_creation %a" concrete_layout_reason concrete

    let intersection_reason ppf = function
      | Gadt_equation p ->
        fprintf ppf "Gadt_equation %a"
          Path.print p
      | Tyvar_refinement ->
        fprintf ppf "Tyvar_refinement"

    let rec history ppf = function
      | Intersection (r, h1, h2) ->
        fprintf ppf "Intersection @[(%a,@ %a,@ %a)@]"
          intersection_reason r
          history h1
          history h2
      | Sublayout (h1, h2) ->
        fprintf ppf "Sublayout @[(%a,@ %a)@]"
          history h1
          history h2
      | Creation c ->
        fprintf ppf "Creation (%a)"
          creation_reason c

    let t ppf ({ layout; history=h } : t) : unit =
      fprintf ppf "@[<v 2>{ layout = %a@,; history = %a }@]"
        internal layout
        history h
  end
end

type layout = Layout.t
