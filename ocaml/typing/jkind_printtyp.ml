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

open Mode
open Jkind_axis

type printable_jkind =
  { base : string;
    modal_bounds : string list
  }

module Bounds = struct
  type t = unit Jkind_types.Layout_and_axes.t

  let of_jkind = Jkind_types.Layout_and_axes.map ~f:(fun _ -> ())
end

let get_modal_bound ~le ~print ~base actual =
  match le actual base with
  | true -> (
    match le base actual with
    | true -> `Valid None
    | false -> `Valid (Some (Format.asprintf "%a" print actual)))
  | false -> `Invalid

let get_modal_bounds ~(base : Bounds.t) (actual : Bounds.t) =
  [ get_modal_bound ~le:Locality.Const.le ~print:Locality.Const.print
      ~base:base.modes_upper_bounds.areality actual.modes_upper_bounds.areality;
    get_modal_bound ~le:Uniqueness.Const.le ~print:Uniqueness.Const.print
      ~base:base.modes_upper_bounds.uniqueness actual.modes_upper_bounds.uniqueness;
    get_modal_bound ~le:Linearity.Const.le ~print:Linearity.Const.print
      ~base:base.modes_upper_bounds.linearity actual.modes_upper_bounds.linearity;
    get_modal_bound ~le:Contention.Const.le ~print:Contention.Const.print
      ~base:base.modes_upper_bounds.contention actual.modes_upper_bounds.contention;
    get_modal_bound ~le:Portability.Const.le ~print:Portability.Const.print
      ~base:base.modes_upper_bounds.portability actual.modes_upper_bounds.portability;
    get_modal_bound ~le:Externality.le ~print:Externality.print
      ~base:base.externality_upper_bound actual.externality_upper_bound;
    get_modal_bound ~le:Nullability.le ~print:Nullability.print
      ~base:base.nullability_upper_bound actual.nullability_upper_bound ]
  |> List.rev
  |> List.fold_left
       (fun acc mode ->
         match acc, mode with
         | _, `Invalid | None, _ -> None
         | acc, `Valid None -> acc
         | Some acc, `Valid (Some mode) -> Some (mode :: acc))
       (Some [])

(** Write [actual] in terms of [base] *)
let convert_with_base ~(base : Jkind.Builtin.Predef.abbrev) (actual : Jkind.Const.t) =
  let matching_layouts =
    Jkind.Layout.Const.equal base.const.layout actual.layout
  in
  let modal_bounds =
    get_modal_bounds
      ~base:(Bounds.of_jkind base.const)
      (Bounds.of_jkind actual)
  in
  match matching_layouts, modal_bounds with
  | true, Some modal_bounds -> Some { base = base.abbrev; modal_bounds }
  | false, _ | _, None -> None

(** Select the out_jkind_const with the least number of modal bounds to print *)
let rec select_simplest = function
  | a :: b :: tl ->
    let simpler =
      if List.length a.modal_bounds < List.length b.modal_bounds
      then a
      else b
    in
    select_simplest (simpler :: tl)
  | [out] -> Some out
  | [] -> None

(* CR layouts v3.0: Remove the [allow_null] treatment here. *)
let to_out_jkind_const ?(allow_null = Language_extension.(is_at_least Layouts Alpha))
     jkind =
  (* For each primitive jkind, we try to print the jkind in terms of it (this is
     possible if the primitive is a subjkind of it). We then choose the "simplest". The
       "simplest" is taken to mean the one with the least number of modes that need to
     follow the [mod]. *)
  let simplest =
    (* CR layouts v3.0: remove this hack once [or_null] is out of [Alpha]. *)
    Jkind.Builtin.Predef.(if allow_null then all_consts else all_consts_non_null)
    |> List.filter_map (fun base -> convert_with_base ~base jkind)
    |> select_simplest
  in
  let printable_jkind =
    match simplest with
    | Some simplest -> simplest
    | None -> (
      (* CR layouts v2.8: sometimes there is no valid way to build a jkind from a
         built-in abbreviation. For now, we just pretend that the layout name is a valid
         jkind abbreviation whose modal bounds are all max, even though this is a
         lie. *)
      let out_jkind_verbose =
        convert_with_base
          ~base:
            { jkind =
                { layout = jkind.layout;
                  modes_upper_bounds = Modes.max;
                  externality_upper_bound = Externality.max;
                  nullability_upper_bound = Nullability.Non_null
                };
              name = Layout.Const.to_string jkind.layout
            }
          jkind
      in
      match out_jkind_verbose with
      | Some out_jkind -> out_jkind
      | None ->
        (* If we fail, try again with nullable jkinds. *)
        let out_jkind_verbose =
          convert_with_base
            ~base:
              { jkind =
                  { layout = jkind.layout;
                    modes_upper_bounds = Modes.max;
                    externality_upper_bound = Externality.max;
                    nullability_upper_bound = Nullability.max
                  };
                name = Layout.Const.to_string jkind.layout
              }
            jkind
        in
        (* convert_with_base is guaranteed to succeed since the layout matches and the
             modal bounds are all max *)
        Option.get out_jkind_verbose)
  in
  match printable_jkind with
  | { base; modal_bounds = _ :: _ as modal_bounds } ->
    Outcometree.Ojkind_const_mod
      (Ojkind_const_abbreviation base, modal_bounds)
  | { base; modal_bounds = [] } ->
    Outcometree.Ojkind_const_abbreviation base

let const ppf jkind = to_out_jkind_const jkind |> !Oprint.out_jkind_const ppf

let const_with_nulls ppf jkind =
  to_out_jkind_const ~allow_null:true jkind
  |> !Oprint.out_jkind_const ppf

let desc ppf =
  let rec pp_element ~nested ppf =
    let open Format in
    function
    | Const c -> fprintf ppf "%a" Const.format c
    | Var v -> fprintf ppf "%s" (Sort.Var.name v)
    | Product ts ->
      let pp_sep ppf () = Format.fprintf ppf "@ & " in
      Misc.pp_nested_list ~nested ~pp_element ~pp_sep ppf ts
  in
  pp_element ~nested:false ppf
