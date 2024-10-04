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

(** Convert a [Jkind.Const.t] into a [Outcometree.out_jkind_const].
    The jkind is written in terms of the built-in jkind that requires the least amount
    of modes after the mod. For example,
    [value mod global many unique portable uncontended external_ non_null] could be
    written in terms of [value] (as it appears above), or in terms of [immediate]
    (which would just be [immediate]). Since the latter requires less modes to be
    printed, it is chosen. *)
val to_out_jkind_const : ?allow_null:bool -> Jkind.Const.t -> Outcometree.out_jkind_const
