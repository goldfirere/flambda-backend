(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Richard Eisenberg, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Jkind_types.Layout_and_axes
open Jkind_axis

let jkind_of_product_annotations jkinds =
  let folder (layouts, mode_ub, ext_ub, null_ub)
      { layout;
        modes_upper_bounds;
        externality_upper_bound;
        nullability_upper_bound
      } =
    ( layout :: layouts,
      Mode.Alloc.Const.join mode_ub modes_upper_bounds,
      Externality.join ext_ub externality_upper_bound,
      Nullability.join null_ub nullability_upper_bound )
  in
  let layouts, mode_ub, ext_ub, null_ub =
    List.fold_left folder
      ([], Mode.Alloc.Const.min, Externality.min, Nullability.min)
      jkinds
  in
  { layout = Product (List.rev layouts);
    modes_upper_bounds = mode_ub;
    externality_upper_bound = ext_ub;
    nullability_upper_bound = null_ub
  }

let rec of_user_written_annotation_unchecked_level
    (jkind : Jane_syntax.Jkind.t) : t =
  match jkind with
  | Abbreviation { txt = name; loc } -> (
    (* CR layouts v2.8: move this to predef *)
    match name with
    (* CR layouts v3.0: remove this hack once non-null jkinds are out of alpha.
       It is confusing, but preserves backwards compatibility for arrays. *)
    | "any" when Language_extension.(is_at_least Layouts Alpha) ->
      Builtin.any.jkind
    | "any" -> Builtin.any_non_null.jkind
    | "any_non_null" -> Builtin.any_non_null.jkind
    | "value_or_null" -> Builtin.value_or_null.jkind
    | "value" -> Builtin.value.jkind
    | "void" -> Builtin.void.jkind
    | "immediate64" -> Builtin.immediate64.jkind
    | "immediate" -> Builtin.immediate.jkind
    | "float64" -> Builtin.float64.jkind
    | "float32" -> Builtin.float32.jkind
    | "word" -> Builtin.word.jkind
    | "bits32" -> Builtin.bits32.jkind
    | "bits64" -> Builtin.bits64.jkind
    | _ -> raise ~loc (Unknown_jkind jkind))
  | Mod (jkind, modifiers) ->
    let base = of_user_written_annotation_unchecked_level jkind in
    (* for each mode, lower the corresponding modal bound to be that mode *)
    let parsed_modifiers = Typemode.transl_modifier_annots modifiers in
    let parsed_modes : Alloc.Const.Option.t =
      { areality = parsed_modifiers.locality;
        linearity = parsed_modifiers.linearity;
        uniqueness = parsed_modifiers.uniqueness;
        portability = parsed_modifiers.portability;
        contention = parsed_modifiers.contention
      }
    in
    { layout = base.layout;
      modes_upper_bounds =
        Alloc.Const.meet base.modes_upper_bounds
          (Alloc.Const.Option.value ~default:Alloc.Const.max parsed_modes);
      nullability_upper_bound =
        Nullability.meet base.nullability_upper_bound
          (Option.value ~default:Nullability.max parsed_modifiers.nullability);
      externality_upper_bound =
        Externality.meet base.externality_upper_bound
          (Option.value ~default:Externality.max parsed_modifiers.externality)
    }
  | Product ts ->
    let jkinds = List.map of_user_written_annotation_unchecked_level ts in
    jkind_of_product_annotations jkinds
  | Default | With _ | Kind_of _ -> Misc.fatal_error "XXX unimplemented"

(* The [annotation_context] parameter can be used to allow annotations / kinds
   in different contexts to be enabled with different extension settings.
   At some points in time, we will not care about the context, and so this
   parameter might effectively be unused.
*)
(* CR layouts: When everything is stable, remove this function. *)
let get_required_layouts_level (_context : History.annotation_context)
    (jkind : t) : Language_extension.maturity =
  match jkind.layout, jkind.nullability_upper_bound with
  | (Base (Float64 | Float32 | Word | Bits32 | Bits64) | Any), _
  | Base Value, Non_null ->
    Stable
  | Base Void, _ | Base Value, Maybe_null -> Alpha
  | Product _, _ -> Beta

let of_user_written_annotation ~context Location.{ loc; txt = annot } =
  let const = of_user_written_annotation_unchecked_level annot in
  let required_layouts_level = get_required_layouts_level context const in
  if not (Language_extension.is_at_least Layouts required_layouts_level)
  then
    raise ~loc (Insufficient_level { jkind = const; required_layouts_level });
  const

let of_annotation ~context (annot : _ Location.loc) =
  let const = Const.of_user_written_annotation ~context annot in
  let jkind = of_annotated_const ~const ~const_loc:annot.loc ~context in
  jkind, (const, annot)

let of_annotation_option_default ~default ~context =
  Option.fold ~none:(default, None) ~some:(fun annot ->
      let t, annot = of_annotation ~context annot in
      t, Some annot)

let of_attribute ~context
    (attribute : Builtin_attributes.jkind_attribute Location.loc) =
  let predef : Jkind.Builtin.Predef.t = match attribute.txt with
    | Immediate -> Immediate
    | Immediate64 -> Immediate64
  in
  let { const; _ } = Jkind.Builtin.Predef.to_abbrev predef in
  of_annotated_const ~context ~const ~const_loc:attribute.loc, const

let of_type_decl ~context (decl : Parsetree.type_declaration) =
  let jkind_of_annotation =
    Jane_syntax.Layouts.of_type_declaration decl
    |> Option.map (fun (annot, attrs) ->
           let t, const = of_annotation ~context annot in
           t, const, attrs)
  in
  let jkind_of_attribute =
    Builtin_attributes.jkind decl.ptype_attributes
    |> Option.map (fun attr ->
           let t, const = of_attribute ~context attr in
           (* This is a bit of a lie: the "annotation" here is being
              forged based on the jkind attribute. But: the jkind
              annotation is just used in printing/untypeast, and the
              all strings valid to use as a jkind attribute are
              valid (and equivalent) to write as an annotation, so
              this lie is harmless.
           *)
           let annot =
             Location.map
               (fun attr ->
                 let name = Builtin_attributes.jkind_attribute_to_string attr in
                 Jane_syntax.Jkind.(Abbreviation (Const.mk name Location.none)))
               attr
           in
           t, (const, annot), decl.ptype_attributes)
  in
  match jkind_of_annotation, jkind_of_attribute with
  | None, None -> None
  | (Some _ as x), None | None, (Some _ as x) -> x
  | Some (_, (from_annotation, _), _), Some (_, (from_attribute, _), _) ->
    raise ~loc:decl.ptype_loc
      (Multiple_jkinds { from_annotation; from_attribute })

let of_type_decl_default ~context ~default (decl : Parsetree.type_declaration) =
  match of_type_decl ~context decl with
  | Some (t, const, attrs) -> t, Some const, attrs
  | None -> default, None, decl.ptype_attributes
