(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Solver
open Solver_intf
open Mode_intf

type nonrec allowed = allowed

type nonrec disallowed = disallowed

(* This module is too general and should be specialized in the future.
   https://github.com/ocaml-flambda/flambda-backend/pull/1760#discussion_r1468531786
*)
module Product = struct
  type ('a0, 'a1) t = 'a0 * 'a1

  (* type aware indexing into a tuple *)
  type ('a0, 'a1, 'a) axis =
    | Axis0 : ('a0, 'a1, 'a0) axis
    | Axis1 : ('a0, 'a1, 'a1) axis

  let print_axis : type a0 a1 a. Format.formatter -> (a0, a1, a) axis -> unit =
   fun ppf -> function
    | Axis0 -> Format.fprintf ppf "0"
    | Axis1 -> Format.fprintf ppf "1"

  let proj (type a0 a1 a) : (a0, a1, a) axis -> a0 * a1 -> a = function
    | Axis0 -> fun (x, _) -> x
    | Axis1 -> fun (_, x) -> x

  let eq_axis (type a0 a1 a b) :
      (a0, a1, a) axis -> (a0, a1, b) axis -> (a, b) Misc.eq option =
   fun a b ->
    match a, b with
    | Axis0, Axis0 -> Some Refl
    | Axis1, Axis1 -> Some Refl
    | _ -> None

  (* Description of which component to set in a product.
     [SAxis0]: update the first element in [('a0, 'a1) t] to get [('b0, 'a1) t].
     [SAxis1]: update the second element in [('a0, 'a1) t] to get [('a0, 'b1) t].
  *)
  type ('a0, 'a1, 'a, 'b0, 'b1, 'b) saxis =
    | SAxis0 : ('a0, 'a1, 'a0, 'b0, 'a1, 'b0) saxis
    | SAxis1 : ('a0, 'a1, 'a1, 'a0, 'b1, 'b1) saxis

  let lift (type a0 a1 a b0 b1 b) :
      (a0, a1, a, b0, b1, b) saxis -> (a -> b) -> (a0, a1) t -> (b0, b1) t =
   fun sax f (a0, a1) ->
    match sax with SAxis0 -> f a0, a1 | SAxis1 -> a0, f a1

  let update (type a0 a1 a) : (a0, a1, a) axis -> a -> a0 * a1 -> a0 * a1 =
    let endo (type a0 a1 a) : (a0, a1, a) axis -> (a0, a1, a, a0, a1, a) saxis =
      function
      | Axis0 -> SAxis0
      | Axis1 -> SAxis1
    in
    fun ax a t -> lift (endo ax) (fun _ -> a) t

  module Lattice (L0 : Lattice) (L1 : Lattice) :
    Lattice with type t = L0.t * L1.t = struct
    type nonrec t = L0.t * L1.t

    let min = L0.min, L1.min

    let max = L0.max, L1.max

    let legacy = L0.legacy, L1.legacy

    let le (a0, a1) (b0, b1) = L0.le a0 b0 && L1.le a1 b1

    let join (a0, a1) (b0, b1) = L0.join a0 b0, L1.join a1 b1

    let meet (a0, a1) (b0, b1) = L0.meet a0 b0, L1.meet a1 b1

    let print ppf (a0, a1) = Format.fprintf ppf "%a,%a" L0.print a0 L1.print a1
  end
end

module Lattices = struct
  module Opposite (L : Lattice) : Lattice with type t = L.t = struct
    type t = L.t

    let min = L.max

    let max = L.min

    let legacy = L.legacy

    let le a b = L.le b a

    let join = L.meet

    let meet = L.join

    let print = L.print
  end

  (* Make the type of [Locality] and [Regionality] below distinguishable,
     so that we can be sure [Comonadic_with] is applied correctly. *)
  module type Areality = sig
    include Lattice

    val _is_areality : unit
  end

  module Locality = struct
    type t =
      | Global
      | Local

    let min = Global

    let max = Local

    let legacy = Global

    let le a b =
      match a, b with Global, _ | _, Local -> true | Local, Global -> false

    let join a b =
      match a, b with Local, _ | _, Local -> Local | Global, Global -> Global

    let meet a b =
      match a, b with Global, _ | _, Global -> Global | Local, Local -> Local

    let print ppf = function
      | Global -> Format.fprintf ppf "Global"
      | Local -> Format.fprintf ppf "Local"

    let _is_areality = ()
  end

  module Regionality = struct
    type t =
      | Global
      | Regional
      | Local

    let min = Global

    let max = Local

    let legacy = Global

    let join a b =
      match a, b with
      | Local, _ | _, Local -> Local
      | Regional, _ | _, Regional -> Regional
      | Global, Global -> Global

    let meet a b =
      match a, b with
      | Global, _ | _, Global -> Global
      | Regional, _ | _, Regional -> Regional
      | Local, Local -> Local

    let le a b =
      match a, b with
      | Global, _ | _, Local -> true
      | _, Global | Local, _ -> false
      | Regional, Regional -> true

    let print ppf = function
      | Global -> Format.fprintf ppf "Global"
      | Regional -> Format.fprintf ppf "Regional"
      | Local -> Format.fprintf ppf "Local"

    let _is_areality = ()
  end

  module Uniqueness = struct
    type t =
      | Unique
      | Shared

    let min = Unique

    let max = Shared

    let legacy = Shared

    let le a b =
      match a, b with Unique, _ | _, Shared -> true | Shared, Unique -> false

    let join a b =
      match a, b with
      | Shared, _ | _, Shared -> Shared
      | Unique, Unique -> Unique

    let meet a b =
      match a, b with
      | Unique, _ | _, Unique -> Unique
      | Shared, Shared -> Shared

    let print ppf = function
      | Shared -> Format.fprintf ppf "Shared"
      | Unique -> Format.fprintf ppf "Unique"
  end

  module Uniqueness_op = Opposite (Uniqueness)

  module Linearity = struct
    type t =
      | Many
      | Once

    let min = Many

    let max = Once

    let legacy = Many

    let le a b =
      match a, b with Many, _ | _, Once -> true | Once, Many -> false

    let join a b =
      match a, b with Once, _ | _, Once -> Once | Many, Many -> Many

    let meet a b =
      match a, b with Many, _ | _, Many -> Many | Once, Once -> Once

    let print ppf = function
      | Once -> Format.fprintf ppf "Once"
      | Many -> Format.fprintf ppf "Many"
  end

  module Externality = struct
    type t =
      | External (* not managed by the garbage collector *)
      | External64 (* not managed by the garbage collector on 64-bit systems *)
      | Internal (* managed by the garbage collector *)

    let min = External

    let max = Internal

    let legacy = Internal

    let le a b =
      match a, b with
      | External, _ -> true
      | _, Internal -> true
      | _, External -> false
      | Internal, _ -> false
      | External64, External64 -> true

    let join a b =
      match a, b with
      | Internal, _ | _, Internal -> Internal
      | External64, _ | _, External64 -> External64
      | External, External -> External

    let meet a b =
      match a, b with
      | External, _ | _, External -> External
      | External64, _ | _, External64 -> External64
      | Internal, Internal -> Internal

    let print ppf = function
      | External -> Format.fprintf ppf "external"
      | External64 -> Format.fprintf ppf "external64"
      | Internal -> Format.fprintf ppf "internal"
  end

  type 'a and_comonads = 'a * Linearity.t * Externality.t

  module Comonadic_with (Areality : Areality) = struct
    type t = Areality.t and_comonads

    let min = Areality.min, Linearity.min, Externality.min

    let max = Areality.max, Linearity.max, Externality.max

    let legacy = Areality.legacy, Linearity.legacy, Externality.legacy

    let le (a0, a1, a2) (b0, b1, b2) =
      Areality.le a0 b0 && Linearity.le a1 b1 && Externality.le a2 b2

    let join (a0, a1, a2) (b0, b1, b2) =
      Areality.join a0 b0, Linearity.join a1 b1, Externality.join a2 b2

    let meet (a0, a1, a2) (b0, b1, b2) =
      Areality.meet a0 b0, Linearity.meet a1 b1, Externality.meet a2 b2

    let print ppf (a0, a1, a2) =
      Format.fprintf ppf "%a,%a,%a" Areality.print a0 Linearity.print a1
        Externality.print a2
  end

  module Comonadic_with_locality = Comonadic_with (Locality)
  module Comonadic_with_regionality = Comonadic_with (Regionality)

  module Areality_obj = struct
  type 'a obj =
    | Locality : Locality.t areality
    | Regionality : Regionality.t areality

  let min : type a. a obj -> a = function
      | Locality -> Locality.min
    | Regionality -> Regionality.min


  let max : type a. a obj -> a = function
    | Locality -> Locality.max
    | Regionality -> Regionality.max

  let le : type a. a obj -> a -> a -> bool = fun obj a b ->
    match obj with
    | Locality -> Locality.le a b
    | Regionality -> Regionality.le a b

  let join : type a. a obj -> a -> a -> a = fun obj a b ->
    match obj with
    | Locality -> Locality.join a b
    | Regionality -> Regionality.join a b

  let meet : type a. a obj -> a -> a -> a = fun obj a b ->
    match obj with
    | Locality -> Locality.meet a b
    | Regionality -> Regionality.meet a b

  let print : type a. _ -> a obj -> unit =
   fun ppf -> function
    | Locality -> Format.fprintf ppf "Locality"
    | Regionality -> Format.fprintf ppf "Regionality"

  include Magic_equal (struct
    type ('a, _, 'd) t = 'a obj constraint 'd = 'l * 'r

    let equal : type a b. a obj -> b obj -> (a, b) Misc.eq option =
     fun a b ->
      match a, b with
      | Locality, Locality -> Some Misc.Refl
      | Regionality, Regionality -> Some Misc.Refl
      | (Locality | Regionality), _ -> None
  end)

end
  type 'a obj =
    | Areality : 'a Areality_obj.obj -> 'a obj
    (* use the flipped version of uniqueness, so that [unique_to_linear] is monotone *)
    | Uniqueness_op : Uniqueness_op.t obj
    | Linearity : Linearity.t obj
    | Externality : Externality.t obj
    | Comonadic_with : 'a Areality_obj.obj -> 'a and_comonads obj


  let print_obj : type a. _ -> a obj -> unit =
   fun ppf -> function
    | Areality a -> Areality_obj.print ppf a
    | Uniqueness_op -> Format.fprintf ppf "Uniqueness_op"
    | Linearity -> Format.fprintf ppf "Linearity"
    | Externality -> Format.fprintf ppf "Externality"
    | Comonadic_with a ->
      Format.fprintf ppf "Comonadic_with_%a" Areality_obj.print a

  let min : type a. a obj -> a = function
    | Areality a -> Areality_obj.min a
    | Uniqueness_op -> Uniqueness_op.min
    | Linearity -> Linearity.min
    | Externality -> Externality.min
    | Comonadic_with Locality -> Comonadic_with_locality.min
    | Comonadic_with Regionality -> Comonadic_with_regionality.min

  let max : type a. a obj -> a = function
    | Areality a -> Areality_obj.max a
    | Uniqueness_op -> Uniqueness_op.max
    | Linearity -> Linearity.max
    | Externality -> Externality.max
    | Comonadic_with Locality -> Comonadic_with_locality.max
    | Comonadic_with Regionality -> Comonadic_with_regionality.max

  let le : type a. a obj -> a -> a -> bool =
   fun obj a b ->
    match obj with
    | Areality area -> Areality_obj.le area a b
    | Uniqueness_op -> Uniqueness_op.le a b
    | Linearity -> Linearity.le a b
    | Externality -> Externality.le a b
    | Comonadic_with Locality -> Comonadic_with_locality.le a b
    | Comonadic_with Regionality -> Comonadic_with_regionality.le a b

  let join : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
   | Areality area -> Areality_obj.join area a b
    | Uniqueness_op -> Uniqueness_op.join a b
    | Linearity -> Linearity.join a b
    | Externality -> Externality.join a b
    | Comonadic_with Locality -> Comonadic_with_locality.join a b
    | Comonadic_with Regionality -> Comonadic_with_regionality.join a b

  let meet : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
   | Areality area -> Areality_obj.meet area a b
    | Uniqueness_op -> Uniqueness_op.meet a b
    | Linearity -> Linearity.meet a b
    | Externality -> Externality.meet a b
    | Comonadic_with Locality -> Comonadic_with_locality.meet a b
    | Comonadic_with Regionality -> Comonadic_with_regionality.meet a b

  (* not hotpath, Ok to curry *)
  let print : type a. a obj -> _ -> a -> unit = function
    | Areality area -> Areality_obj.print area
    | Uniqueness_op -> Uniqueness_op.print
    | Linearity -> Linearity.print
    | Externality -> Externality.print
    | Comonadic_with Locality -> Comonadic_with_locality.print
    | Comonadic_with Regionality -> Comonadic_with_regionality.print

  module Equal_obj = Magic_equal (struct
    type ('a, _, 'd) t = 'a obj constraint 'd = 'l * 'r

    let equal : type a b. a obj -> b obj -> (a, b) Misc.eq option =
     fun a b ->
      match a, b with
      | Areality a0, Areality a1 -> (
        match eq_areality a0 a1 with Some Refl -> Some Refl | None -> None)
      | Uniqueness_op, Uniqueness_op -> Some Misc.Refl
      | Linearity, Linearity -> Some Misc.Refl
      | Externality, Externality -> Some Misc.Refl
      | Comonadic_with a0, Comonadic_with a1 -> (
        match eq_areality a0 a1 with Some Refl -> Some Refl | None -> None)
      | ( ( Areality _ | Uniqueness_op | Linearity | Externality
          | Comonadic_with _ ),
          _ ) ->
        None
  end)

  let eq_obj = Equal_obj.equal
end

module Lattices_mono = struct
  include Lattices

  type ('a, 'b, 'd) morph =
    | Id : ('a, 'a, 'd) morph  (** identity morphism *)
    | Const_min : 'a obj -> ('a, 'b, 'd * disallowed) morph
        (** The constant morphism that always maps to the minimum *)
    | Const_max : 'a obj -> ('a, 'b, disallowed * 'd) morph
        (** The constant morphism that always maps to the maximum *)
    | Proj :
        ('a0, 'a1) Product.t obj * ('a0, 'a1, 'a) Product.axis
        -> (('a0, 'a1) Product.t, 'a, 'l * 'r) morph
        (** projection from product to an axis *)
    | Max_with_areality : ('a, 'a and_comonads, disallowed * 'r) morph
        (** Combine an areality with maxima along other comonadic axes *)
    | Max_with_linearity : (Linearity.t, 'a and_comonads, disallowed * 'r) morph
        (** Combine linearity with maxima along other comonadic axes *)
    | Max_with_externality
        : (Externality.t, 'a and_comonads, disallowed * 'r) morph
        (** Combine externality with maxima along other comonadic axes *)
    | Min_with_areality : ('a, 'a and_comonads, 'l * disallowed) morph
        (** Combine an areality with minima along other comonadic axes *)
    | Min_with_linearity : (Linearity.t, 'a and_comonads, 'l * disallowed) morph
        (** Combine linearity with minima along other comonadic axes *)
    | Min_with_externality
        : (Externality.t, 'a and_comonads, 'l * disallowed) morph
        (** Combine externality with minima along other comonadic axes *)
    | Map :
        ('area, 'area, 'd) morph
        * (Linearity.t, Linearity.t, 'd) morph
        * (Externality.t, Externality.t, 'd) morph
        -> ('area and_comonads, 'area and_comonads, 'd) morph
        (** Maps the comonads per-axis *)
    | Unique_to_linear : (Uniqueness_op.t, Linearity.t, 'l * 'r) morph
        (** Returns the linearity dual to the given uniqueness *)
    | Linear_to_unique : (Linearity.t, Uniqueness_op.t, 'l * 'r) morph
        (** Returns the uniqueness dual to the given linearity *)
    (* Following is a chain of adjunction (complete and cannot extend in
       either direction) *)
    | Local_to_regional : (Locality.t, Regionality.t, 'l * disallowed) morph
        (** Maps local to regional, global to global *)
    | Regional_to_local : (Regionality.t, Locality.t, 'l * 'r) morph
        (** Maps regional to local, identity otherwise *)
    | Locality_as_regionality : (Locality.t, Regionality.t, 'l * 'r) morph
        (** Inject locality into regionality  *)
    | Regional_to_global : (Regionality.t, Locality.t, 'l * 'r) morph
        (** Maps regional to global, identity otherwise *)
    | Global_to_regional : (Locality.t, Regionality.t, disallowed * 'r) morph
        (** Maps global to regional, local to local *)
    | Compose : ('b, 'c, 'd) morph * ('a, 'b, 'd) morph -> ('a, 'c, 'd) morph
        (** Compoistion of two morphisms *)

  include Magic_allow_disallow (struct
    type ('a, 'b, 'd) sided = ('a, 'b, 'd) morph constraint 'd = 'l * 'r

    let rec allow_left :
        type a b l r. (a, b, allowed * r) morph -> (a, b, l * r) morph =
      function
      | Id -> Id
      | Proj (src, ax) -> Proj (src, ax)
      | Min_with_areality -> Min_with_areality
      | Min_with_linearity -> Min_with_linearity
      | Min_with_externality -> Min_with_externality
      | Const_min src -> Const_min src
      | Compose (f, g) ->
        let f = allow_left f in
        let g = allow_left g in
        Compose (f, g)
      | Unique_to_linear -> Unique_to_linear
      | Linear_to_unique -> Linear_to_unique
      | Local_to_regional -> Local_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Regional_to_global -> Regional_to_global
      | Map (f0, f1, f2) ->
        let f0 = allow_left f0 in
        let f1 = allow_left f1 in
        let f2 = allow_left f2 in
        Map (f0, f1, f2)

    let rec allow_right :
        type a b l r. (a, b, l * allowed) morph -> (a, b, l * r) morph =
      function
      | Id -> Id
      | Proj (src, ax) -> Proj (src, ax)
      | Max_with_areality -> Max_with_areality
      | Max_with_linearity -> Max_with_linearity
      | Max_with_externality -> Max_with_externality
      | Const_max src -> Const_max src
      | Compose (f, g) ->
        let f = allow_right f in
        let g = allow_right g in
        Compose (f, g)
      | Unique_to_linear -> Unique_to_linear
      | Linear_to_unique -> Linear_to_unique
      | Global_to_regional -> Global_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Regional_to_global -> Regional_to_global
      | Map (f0, f1, f2) ->
        let f0 = allow_right f0 in
        let f1 = allow_right f1 in
        let f2 = allow_right f2 in
        Map (f0, f1, f2)

    let rec disallow_left :
        type a b l r. (a, b, l * r) morph -> (a, b, disallowed * r) morph =
      function
      | Id -> Id
      | Proj (src, ax) -> Proj (src, ax)
      | Min_with_areality -> Min_with_areality
      | Min_with_linearity -> Min_with_linearity
      | Min_with_externality -> Min_with_externality
      | Max_with_areality -> Max_with_areality
      | Max_with_linearity -> Max_with_linearity
      | Max_with_externality -> Max_with_externality
      | Const_max src -> Const_max src
      | Const_min src -> Const_min src
      | Compose (f, g) ->
        let f = disallow_left f in
        let g = disallow_left g in
        Compose (f, g)
      | Unique_to_linear -> Unique_to_linear
      | Linear_to_unique -> Linear_to_unique
      | Local_to_regional -> Local_to_regional
      | Global_to_regional -> Global_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Regional_to_global -> Regional_to_global
      | Map (f0, f1, f2) ->
        let f0 = disallow_left f0 in
        let f1 = disallow_left f1 in
        let f2 = disallow_left f2 in
        Map (f0, f1, f2)

    let rec disallow_right :
        type a b l r. (a, b, l * r) morph -> (a, b, l * disallowed) morph =
      function
      | Id -> Id
      | Proj (src, ax) -> Proj (src, ax)
      | Min_with_areality -> Min_with_areality
      | Min_with_linearity -> Min_with_linearity
      | Min_with_externality -> Min_with_externality
      | Max_with_areality -> Max_with_areality
      | Max_with_linearity -> Max_with_linearity
      | Max_with_externality -> Max_with_externality
      | Const_max src -> Const_max src
      | Const_min src -> Const_min src
      | Compose (f, g) ->
        let f = disallow_right f in
        let g = disallow_right g in
        Compose (f, g)
      | Unique_to_linear -> Unique_to_linear
      | Linear_to_unique -> Linear_to_unique
      | Local_to_regional -> Local_to_regional
      | Global_to_regional -> Global_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Regional_to_global -> Regional_to_global
      | Map (f0, f1, f2) ->
        let f0 = disallow_right f0 in
        let f1 = disallow_right f1 in
        let f2 = disallow_right f2 in
        Map (f0, f1, f2)
  end)

  let rec src : type a b d. b obj -> (a, b, d) morph -> a obj =
   fun dst f ->
    let areality_src (type area) (dst : area and_comonads obj) =
      match dst with Comonadic_with a -> a
    in
    match f with
    | Id -> dst
    | Proj (src, _) -> src
    | Max_with_areality -> Areality (areality_src dst)
    | Max_with_linearity -> Linearity
    | Max_with_externality -> Externality
    | Min_with_areality -> Areality (areality_src dst)
    | Min_with_linearity -> Linearity
    | Min_with_externality -> Externality
    | Const_min src | Const_max src -> src
    | Compose (f, g) ->
      let mid = src dst f in
      src mid g
    | Unique_to_linear -> Uniqueness_op
    | Linear_to_unique -> Linearity
    | Local_to_regional -> Areality Locality
    | Locality_as_regionality -> Areality Locality
    | Global_to_regional -> Areality Locality
    | Regional_to_local -> Areality Regionality
    | Regional_to_global -> Areality Regionality
    | Map _ -> Comonadic_with (areality_src dst)

  module Equal_morph = Magic_equal (struct
    type ('a, 'b, 'd) t = ('a, 'b, 'd) morph constraint 'd = 'l * 'r

    let rec equal :
        type a0 l0 r0 a1 b l1 r1.
        (a0, b, l0 * r0) morph ->
        (a1, b, l1 * r1) morph ->
        (a0, a1) Misc.eq option =
     fun f0 f1 ->
      match f0, f1 with
      | Id, Id -> Some Refl
      | Proj (src0, ax0), Proj (src1, ax1) -> (
        match eq_obj src0 src1 with
        | Some Refl -> (
          match Product.eq_axis ax0 ax1 with
          | None -> None
          | Some Refl -> Some Refl)
        | None -> None)
      | Max_with_areality, Max_with_areality -> Some Refl
      | Max_with_linearity, Max_with_linearity -> Some Refl
      | Max_with_externality, Max_with_externality -> Some Refl
      | Min_with_areality, Min_with_areality -> Some Refl
      | Min_with_linearity, Min_with_linearity -> Some Refl
      | Min_with_externality, Min_with_externality -> Some Refl
      | Const_min src0, Const_min src1 -> (
        match eq_obj src0 src1 with Some Refl -> Some Refl | None -> None)
      | Const_max src0, Const_max src1 -> (
        match eq_obj src0 src1 with Some Refl -> Some Refl | None -> None)
      | Unique_to_linear, Unique_to_linear -> Some Refl
      | Linear_to_unique, Linear_to_unique -> Some Refl
      | Local_to_regional, Local_to_regional -> Some Refl
      | Locality_as_regionality, Locality_as_regionality -> Some Refl
      | Global_to_regional, Global_to_regional -> Some Refl
      | Regional_to_local, Regional_to_local -> Some Refl
      | Regional_to_global, Regional_to_global -> Some Refl
      | Compose (f0, g0), Compose (f1, g1) -> (
        match equal f0 f1 with
        | None -> None
        | Some Refl -> (
          match equal g0 g1 with None -> None | Some Refl -> Some Refl))
      | Map (f0, f1, f2), Map (g0, g1, g2) -> (
        match equal f0 g0, equal f1 g1, equal f2 g2 with
        | Some Refl, Some Refl, Some Refl -> Some Refl
        | _, _, _ -> None)
      | ( ( Id | Proj _ | Max_with_areality | Max_with_linearity
          | Max_with_externality | Min_with_areality | Min_with_linearity
          | Min_with_externality | Const_min _ | Const_max _ | Unique_to_linear
          | Linear_to_unique | Local_to_regional | Locality_as_regionality
          | Global_to_regional | Regional_to_local | Regional_to_global
          | Compose _ | Map _ ),
          _ ) ->
        None
  end)

  let eq_morph = Equal_morph.equal

  let rec print_morph : type a b d. Format.formatter -> (a, b, d) morph -> unit
      =
   fun ppf -> function
    | Id -> Format.fprintf ppf "id"
    | Const_min _ -> Format.fprintf ppf "const_min"
    | Const_max _ -> Format.fprintf ppf "const_max"
    | Proj (_, ax) -> Format.fprintf ppf "proj_%a" Product.print_axis ax
    | Max_with_areality -> Format.fprintf ppf "max_with_areality"
    | Max_with_linearity -> Format.fprintf ppf "max_with_linearity"
    | Max_with_externality -> Format.fprintf ppf "max_with_externality"
    | Min_with_areality -> Format.fprintf ppf "min_with_areality"
    | Min_with_linearity -> Format.fprintf ppf "min_with_linearity"
    | Min_with_externality -> Format.fprintf ppf "min_with_externality"
    | Map (f0, f1, f2) ->
      Format.fprintf ppf "map(%a,%a)" print_morph f0 print_morph f1
    | Unique_to_linear -> Format.fprintf ppf "unique_to_linear"
    | Linear_to_unique -> Format.fprintf ppf "linear_to_unique"
    | Local_to_regional -> Format.fprintf ppf "local_to_regional"
    | Regional_to_local -> Format.fprintf ppf "regional_to_local"
    | Locality_as_regionality -> Format.fprintf ppf "locality_as_regionality"
    | Regional_to_global -> Format.fprintf ppf "regional_to_global"
    | Global_to_regional -> Format.fprintf ppf "global_to_regional"
    | Compose (f0, f1) ->
      Format.fprintf ppf "%a ∘ %a" print_morph f0 print_morph f1

  let id = Id

  let linear_to_unique = function
    | Linearity.Many -> Uniqueness.Shared
    | Linearity.Once -> Uniqueness.Unique

  let unique_to_linear = function
    | Uniqueness.Unique -> Linearity.Once
    | Uniqueness.Shared -> Linearity.Many

  let local_to_regional = function
    | Locality.Global -> Regionality.Global
    | Locality.Local -> Regionality.Regional

  let regional_to_local = function
    | Regionality.Local -> Locality.Local
    | Regionality.Regional -> Locality.Local
    | Regionality.Global -> Locality.Global

  let locality_as_regionality = function
    | Locality.Local -> Regionality.Local
    | Locality.Global -> Regionality.Global

  let regional_to_global = function
    | Regionality.Local -> Locality.Local
    | Regionality.Regional -> Locality.Global
    | Regionality.Global -> Locality.Global

  let global_to_regional = function
    | Locality.Local -> Regionality.Local
    | Locality.Global -> Regionality.Regional

  let rec apply : type a b d. b obj -> (a, b, d) morph -> a -> b =
   fun dst f a ->
    match f with
    | Compose (f, g) ->
      let mid = src dst f in
      let g' = apply mid g in
      let f' = apply dst f in
      f' (g' a)
    | Id -> a
    | Proj (_, ax) -> Product.proj ax a
    | Max_with_areality -> a, Linearity.max, Externality.max
    | Max_with_linearity ->
    | Max_with ax -> Product.update ax a (max dst)
    | Min_with ax -> Product.update ax a (min dst)
    | Const_min _ -> min dst
    | Const_max _ -> max dst
    | Unique_to_linear -> unique_to_linear a
    | Linear_to_unique -> linear_to_unique a
    | Local_to_regional -> local_to_regional a
    | Regional_to_local -> regional_to_local a
    | Locality_as_regionality -> locality_as_regionality a
    | Regional_to_global -> regional_to_global a
    | Global_to_regional -> global_to_regional a
    | Map (f0, f1) ->
      let dst0 = proj_obj Axis0 dst in
      let dst1 = proj_obj Axis1 dst in
      let a0, a1 = a in
      apply dst0 f0 a0, apply dst1 f1 a1

  (** Compose m0 after m1. Returns [Some f] if the composition can be
    represented by [f] instead of [Compose m0 m1]. [None] otherwise. *)
  let rec maybe_compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph option =
   fun dst m0 m1 ->
    match m0, m1 with
    | Id, m -> Some m
    | m, Id -> Some m
    | Compose (f0, f1), g -> (
      let mid = src dst f0 in
      match maybe_compose mid f1 g with
      | Some m -> Some (compose dst f0 m)
      (* the check needed to prevent infinite loop *)
      | None -> None)
    | f, Compose (g0, g1) -> (
      match maybe_compose dst f g0 with
      | Some m -> Some (compose dst m g1)
      | None -> None)
    | Const_min mid, f -> Some (Const_min (src mid f))
    | Const_max mid, f -> Some (Const_max (src mid f))
    | Proj _, Const_min src -> Some (Const_min src)
    | Proj _, Const_max src -> Some (Const_max src)
    | Proj (mid, ax0), Max_with ax1 -> (
      match Product.eq_axis ax0 ax1 with
      | None -> Some (Const_max (proj_obj ax1 mid))
      | Some Refl -> Some Id)
    | Proj (mid, ax0), Min_with ax1 -> (
      match Product.eq_axis ax0 ax1 with
      | None -> Some (Const_min (proj_obj ax1 mid))
      | Some Refl -> Some Id)
    | Proj (mid, ax), Map (f0, f1) -> (
      let src' = src mid m1 in
      match ax with
      | Axis0 -> Some (compose dst f0 (Proj (src', Axis0)))
      | Axis1 -> Some (compose dst f1 (Proj (src', Axis1))))
    | Max_with _, Const_max src -> Some (Const_max src)
    | Min_with _, Const_min src -> Some (Const_min src)
    | Unique_to_linear, Const_min src -> Some (Const_min src)
    | Linear_to_unique, Const_min src -> Some (Const_min src)
    | Unique_to_linear, Const_max src -> Some (Const_max src)
    | Linear_to_unique, Const_max src -> Some (Const_max src)
    | Unique_to_linear, Linear_to_unique -> Some Id
    | Linear_to_unique, Unique_to_linear -> Some Id
    | Map (f0, f1), Map (g0, g1) ->
      let dst0 = proj_obj Axis0 dst in
      let dst1 = proj_obj Axis1 dst in
      Some (Map (compose dst0 f0 g0, compose dst1 f1 g1))
    | Regional_to_local, Local_to_regional -> Some Id
    | Regional_to_local, Global_to_regional ->
      Some (Const_max (Areality Locality))
    | Regional_to_local, Const_min src -> Some (Const_min src)
    | Regional_to_local, Const_max src -> Some (Const_max src)
    | Regional_to_local, Locality_as_regionality -> Some Id
    | Regional_to_global, Locality_as_regionality -> Some Id
    | Regional_to_global, Local_to_regional ->
      Some (Const_min (Areality Locality))
    | Regional_to_global, Const_min src -> Some (Const_min src)
    | Regional_to_global, Const_max src -> Some (Const_max src)
    | Local_to_regional, Regional_to_local -> None
    | Local_to_regional, Regional_to_global -> None
    | Local_to_regional, Const_min src -> Some (Const_min src)
    | Local_to_regional, Const_max _ -> None
    | Locality_as_regionality, Regional_to_local -> None
    | Locality_as_regionality, Regional_to_global -> None
    | Locality_as_regionality, Const_min src -> Some (Const_min src)
    | Locality_as_regionality, Const_max _ -> None
    | Global_to_regional, Regional_to_local -> None
    | Regional_to_global, Global_to_regional -> Some Id
    | Global_to_regional, Regional_to_global -> None
    | Global_to_regional, Const_min _ -> None
    | Global_to_regional, Const_max src -> Some (Const_max src)
    | Min_with _, _ -> None
    | Max_with _, _ -> None
    | _, Proj _ -> None
    | Map _, _ -> None

  and compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph =
   fun dst f g ->
    match maybe_compose dst f g with Some m -> m | None -> Compose (f, g)

  let rec left_adjoint :
      type a b l.
      b obj -> (a, b, l * allowed) morph -> (b, a, allowed * disallowed) morph =
   fun dst f ->
    match f with
    | Id -> Id
    | Proj (_, ax) -> Min_with ax
    | Max_with ax -> Proj (dst, ax)
    | Compose (f, g) ->
      let mid = src dst f in
      let f' = left_adjoint dst f in
      let g' = left_adjoint mid g in
      Compose (g', f')
    | Const_max _ -> Const_min dst
    | Unique_to_linear -> Linear_to_unique
    | Linear_to_unique -> Unique_to_linear
    | Global_to_regional -> Regional_to_global
    | Regional_to_global -> Locality_as_regionality
    | Locality_as_regionality -> Regional_to_local
    | Regional_to_local -> Local_to_regional
    | Map (f0, f1) ->
      let dst0 = proj_obj Axis0 dst in
      let dst1 = proj_obj Axis1 dst in
      let f0' = left_adjoint dst0 f0 in
      let f1' = left_adjoint dst1 f1 in
      Map (f0', f1')

  and right_adjoint :
      type a b r.
      b obj -> (a, b, allowed * r) morph -> (b, a, disallowed * allowed) morph =
   fun dst f ->
    match f with
    | Id -> Id
    | Proj (_, ax) -> Max_with ax
    | Min_with ax -> Proj (dst, ax)
    | Compose (f, g) ->
      let mid = src dst f in
      let f' = right_adjoint dst f in
      let g' = right_adjoint mid g in
      Compose (g', f')
    | Const_min _ -> Const_max dst
    | Unique_to_linear -> Linear_to_unique
    | Linear_to_unique -> Unique_to_linear
    | Local_to_regional -> Regional_to_local
    | Regional_to_local -> Locality_as_regionality
    | Locality_as_regionality -> Regional_to_global
    | Regional_to_global -> Global_to_regional
    | Map (f0, f1) ->
      let dst0 = proj_obj Axis0 dst in
      let dst1 = proj_obj Axis1 dst in
      let f0' = right_adjoint dst0 f0 in
      let f1' = right_adjoint dst1 f1 in
      Map (f0', f1')

  (** Helper functions that returns a [Map] that corresponds to lifting *)
  let lift (type a0 a1 a b0 b1 b d) :
      (a0, a1, a, b0, b1, b) Product.saxis ->
      (a, b, d) morph ->
      ((a0, a1) Product.t, (b0, b1) Product.t, d) morph =
   fun sax f ->
    match sax, f with SAxis0, f0 -> Map (f0, Id) | SAxis1, f1 -> Map (Id, f1)
end

module C = Lattices_mono
module S = Solvers_polarized (C)

type changes = S.changes

let undo_changes = S.undo_changes

let set_append_changes = S.set_append_changes

(** Representing a single object *)
module type Obj = sig
  type const

  module Solver : S.Solver_polarized

  val obj : const C.obj
end

let equate_from_submode submode m0 m1 =
  match submode m0 m1 with
  | Error e -> Error (Left_le_right, e)
  | Ok () -> (
    match submode m1 m0 with
    | Error e -> Error (Right_le_left, e)
    | Ok () -> Ok ())
  [@@inline]

module Common (Obj : Obj) = struct
  open Obj

  type 'd t = (const, 'd) Solver.mode

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type nonrec error = const error

  type equate_error = equate_step * error

  type (_, _, 'd) sided = 'd t

  let disallow_right m = Solver.disallow_right m

  let disallow_left m = Solver.disallow_left m

  let allow_left m = Solver.allow_left m

  let allow_right m = Solver.allow_right m

  let newvar () = Solver.newvar obj

  let min = Solver.min obj

  let max = Solver.max obj

  let newvar_above m = Solver.newvar_above obj m

  let newvar_below m = Solver.newvar_below obj m

  let submode m0 m1 : (unit, error) result = Solver.submode obj m0 m1

  let join l = Solver.join obj l

  let meet l = Solver.meet obj l

  let submode_exn m0 m1 = assert (submode m0 m1 |> Result.is_ok)

  let equate = equate_from_submode submode

  let equate_exn m0 m1 = assert (equate m0 m1 |> Result.is_ok)

  let print ?(raw = false) ?verbose () ppf m =
    if raw
    then Solver.print_raw ?verbose obj ppf m
    else Solver.print ?verbose obj ppf m

  let zap_to_ceil m = Solver.zap_to_ceil obj m

  let zap_to_floor m = Solver.zap_to_floor obj m

  let of_const : type l r. const -> (l * r) t = fun a -> Solver.of_const obj a

  let check_const m = Solver.check_const obj m
end
[@@inline]

module Locality = struct
  module Const = C.Locality

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let areality = C.Locality

    let obj = C.Areality areality
  end

  include Common (Obj)

  let global = of_const Global

  let local = of_const Local

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Regionality = struct
  module Const = C.Regionality

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let areality = C.Regionality

    let obj = C.Areality areality
  end

  include Common (Obj)

  let local = of_const Const.Local

  let regional = of_const Const.Regional

  let global = of_const Const.Global

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Linearity = struct
  module Const = C.Linearity

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let obj = C.Linearity
  end

  include Common (Obj)

  let many = of_const Many

  let once = of_const Once

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Uniqueness = struct
  module Const = C.Uniqueness

  module Obj = struct
    type const = Const.t

    (* the negation of Uniqueness_op gives us the proper uniqueness *)
    module Solver = S.Negative

    let obj = C.Uniqueness_op
  end

  include Common (Obj)

  let shared = of_const Shared

  let unique = of_const Unique

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_ceil
end

let unique_to_linear m =
  S.Positive.via_antitone Linearity.Obj.obj C.Unique_to_linear m

let linear_to_unique m =
  S.Negative.via_antitone Uniqueness.Obj.obj C.Linear_to_unique m

let regional_to_local m =
  S.Positive.via_monotone Locality.Obj.obj C.Regional_to_local m

let locality_as_regionality m =
  S.Positive.via_monotone Regionality.Obj.obj C.Locality_as_regionality m

let regional_to_global m =
  S.Positive.via_monotone Locality.Obj.obj C.Regional_to_global m

module type Areality = sig
  module Const : C.Areality

  module Obj : sig
    include Obj with type const = Const.t and module Solver = S.Positive

    val areality : Const.t C.areality
  end

  type 'd t = (Const.t, 'd) Obj.Solver.mode

  type error

  val check_const : ('l * 'r) t -> Const.t option
end

module Comonadic_with (Areality : Areality) = struct
  module Const = C.Comonadic_with (Areality.Const)

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let obj = C.Comonadic_with Areality.Obj.areality
  end

  include Common (Obj)

  type error =
    [ `Areality of Areality.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  let areality m =
    S.Positive.via_monotone Areality.Obj.obj (C.Proj (Obj.obj, Axis0)) m

  let min_with_areality m =
    S.Positive.via_monotone Obj.obj (C.Min_with Axis0)
      (S.Positive.disallow_right m)

  let max_with_areality m =
    S.Positive.via_monotone Obj.obj (C.Max_with Axis0)
      (S.Positive.disallow_left m)

  let set_areality_max m =
    S.Positive.via_monotone Obj.obj
      (C.lift Product.SAxis0 (C.Const_max Areality.Obj.obj))
      (S.Positive.disallow_left m)

  let set_areality_min m =
    S.Positive.via_monotone Obj.obj
      (C.lift Product.SAxis0 (C.Const_min Areality.Obj.obj))
      (S.Positive.disallow_right m)

  let linearity m =
    S.Positive.via_monotone Linearity.Obj.obj (C.Proj (Obj.obj, Axis1)) m

  let min_with_linearity m =
    S.Positive.via_monotone Obj.obj (C.Min_with Axis1)
      (S.Positive.disallow_right m)

  let max_with_linearity m =
    S.Positive.via_monotone Obj.obj (C.Max_with Axis1)
      (S.Positive.disallow_left m)

  let set_linearity_max m =
    S.Positive.via_monotone Obj.obj
      (C.lift Product.SAxis1 (C.Const_max Linearity))
      (S.Positive.disallow_left m)

  let set_linearity_min m =
    S.Positive.via_monotone Obj.obj
      (C.lift Product.SAxis1 (C.Const_min Linearity))
      (S.Positive.disallow_right m)

  let zap_to_legacy = zap_to_floor

  let legacy = of_const Const.legacy

  (* overriding to report the offending axis *)
  let submode m0 m1 =
    match submode m0 m1 with
    | Ok () -> Ok ()
    | Error { left = area0, lin0; right = area1, lin1 } ->
      if Areality.Const.le area0 area1
      then
        if Linearity.Const.le lin0 lin1
        then assert false
        else Error (`Linearity { left = lin0; right = lin1 })
      else Error (`Areality { left = area0; right = area1 })

  (* override to report the offending axis *)
  let equate = equate_from_submode submode

  (** overriding to check per-axis *)
  let check_const m =
    let areality = Areality.check_const (areality m) in
    let linearity = Linearity.check_const (linearity m) in
    areality, linearity
end

module Monadic = struct
  let uniqueness m = m

  (* secretly just uniqueness *)
  include Uniqueness

  type error = [`Uniqueness of Uniqueness.error]

  type equate_error = equate_step * error

  let max_with_uniqueness m = S.Negative.disallow_left m

  let min_with_uniqueness m = S.Negative.disallow_right m

  let set_uniqueness_max _ =
    Uniqueness.max |> S.Negative.disallow_left |> S.Negative.allow_right

  let set_uniqueness_min _ =
    Uniqueness.min |> S.Negative.disallow_right |> S.Negative.allow_left

  let submode m0 m1 =
    match submode m0 m1 with Ok () -> Ok () | Error e -> Error (`Uniqueness e)

  let equate = equate_from_submode submode
end

type ('mo, 'como) monadic_comonadic =
  { monadic : 'mo;
    comonadic : 'como
  }

module Mode (Areality : Areality) = struct
  module Comonadic = Comonadic_with (Areality)
  module Monadic = Monadic

  type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  let min = { comonadic = Comonadic.min; monadic = Monadic.min }

  let max =
    { comonadic = Comonadic.max;
      monadic = Monadic.max |> Monadic.allow_left |> Monadic.allow_right
    }

  include Magic_allow_disallow (struct
    type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

    let allow_left { monadic; comonadic } =
      let monadic = Monadic.allow_left monadic in
      let comonadic = Comonadic.allow_left comonadic in
      { monadic; comonadic }

    let allow_right { monadic; comonadic } =
      let monadic = Monadic.allow_right monadic in
      let comonadic = Comonadic.allow_right comonadic in
      { monadic; comonadic }

    let disallow_left { monadic; comonadic } =
      let monadic = Monadic.disallow_left monadic in
      let comonadic = Comonadic.disallow_left comonadic in
      { monadic; comonadic }

    let disallow_right { monadic; comonadic } =
      let monadic = Monadic.disallow_right monadic in
      let comonadic = Comonadic.disallow_right comonadic in
      { monadic; comonadic }
  end)

  let newvar () =
    let comonadic = Comonadic.newvar () in
    let monadic = Monadic.newvar () in
    { comonadic; monadic }

  let newvar_above { comonadic; monadic } =
    let comonadic, b0 = Comonadic.newvar_above comonadic in
    let monadic, b1 = Monadic.newvar_above monadic in
    { monadic; comonadic }, b0 || b1

  let newvar_below { comonadic; monadic } =
    let comonadic, b0 = Comonadic.newvar_below comonadic in
    let monadic, b1 = Monadic.newvar_below monadic in
    { monadic; comonadic }, b0 || b1

  let uniqueness { monadic; _ } = Monadic.uniqueness monadic

  let linearity { comonadic; _ } = Comonadic.linearity comonadic

  let areality { comonadic; _ } = Comonadic.areality comonadic

  type error =
    [ `Areality of Areality.error
    | `Uniqueness of Uniqueness.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  (* NB: state mutated when error *)
  let submode { monadic = monadic0; comonadic = comonadic0 }
      { monadic = monadic1; comonadic = comonadic1 } =
    (* comonadic before monadic, so that locality errors dominate
       (error message backward compatibility) *)
    match Comonadic.submode comonadic0 comonadic1 with
    | Error e -> Error e
    | Ok () -> (
      match Monadic.submode monadic0 monadic1 with
      | Error e -> Error e
      | Ok () -> Ok ())

  let equate = equate_from_submode submode

  let submode_exn m0 m1 =
    match submode m0 m1 with
    | Ok () -> ()
    | Error _ -> invalid_arg "submode_exn"

  let equate_exn m0 m1 =
    match equate m0 m1 with Ok () -> () | Error _ -> invalid_arg "equate_exn"

  let print ?raw ?verbose () ppf { monadic; comonadic } =
    Format.fprintf ppf "%a,%a"
      (Comonadic.print ?raw ?verbose ())
      comonadic
      (Monadic.print ?raw ?verbose ())
      monadic

  let legacy =
    let comonadic = Comonadic.legacy in
    let monadic = Monadic.legacy in
    { comonadic; monadic }

  let max_with_uniqueness uniqueness =
    let comonadic =
      Comonadic.max |> Comonadic.disallow_left |> Comonadic.allow_right
    in
    let monadic = Monadic.max_with_uniqueness uniqueness in
    { comonadic; monadic }

  let min_with_uniqueness uniqueness =
    let comonadic =
      Comonadic.min |> Comonadic.disallow_right |> Comonadic.allow_left
    in
    let monadic = Monadic.min_with_uniqueness uniqueness in
    { comonadic; monadic }

  let set_uniqueness_max { monadic; comonadic } =
    let comonadic = Comonadic.disallow_left comonadic in
    let monadic = Monadic.set_uniqueness_max monadic in
    { monadic; comonadic }

  let set_uniqueness_min { monadic; comonadic } =
    let comonadic = Comonadic.disallow_right comonadic in
    let monadic = Monadic.set_uniqueness_min monadic in
    { monadic; comonadic }

  let min_with_areality areality =
    let comonadic = Comonadic.min_with_areality areality in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_areality areality =
    let comonadic = Comonadic.max_with_areality areality in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let set_areality_min { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.set_areality_min comonadic in
    { comonadic; monadic }

  let set_areality_max { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.set_areality_max comonadic in
    { comonadic; monadic }

  let min_with_linearity linearity =
    let comonadic = Comonadic.min_with_linearity linearity in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_linearity linearity =
    let comonadic = Comonadic.max_with_linearity linearity in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let set_linearity_max { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.set_linearity_max comonadic in
    { comonadic; monadic }

  let set_linearity_min { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.set_linearity_min comonadic in
    { comonadic; monadic }

  let join l =
    let como, mo =
      List.fold_left
        (fun (como, mo) { comonadic; monadic } ->
          comonadic :: como, monadic :: mo)
        ([], []) l
    in
    let comonadic = Comonadic.join como in
    let monadic = Monadic.join mo in
    { comonadic; monadic }

  let meet l =
    let como, mo =
      List.fold_left
        (fun (como, mo) { comonadic; monadic } ->
          comonadic :: como, monadic :: mo)
        ([], []) l
    in
    let comonadic = Comonadic.meet como in
    let monadic = Monadic.meet mo in
    { comonadic; monadic }

  module Const = struct
    type ('area, 'lin, 'uni) modes =
      { areality : 'area;
        linearity : 'lin;
        uniqueness : 'uni
      }

    type t = (Areality.Const.t, Linearity.Const.t, Uniqueness.Const.t) modes

    let of_const { areality; linearity; uniqueness } =
      let comonadic = Comonadic.of_const (areality, linearity) in
      let monadic = Monadic.of_const uniqueness in
      { comonadic; monadic }

    let min =
      let areality = Areality.Const.min in
      let linearity = Linearity.Const.min in
      let uniqueness = Uniqueness.Const.min in
      { areality; linearity; uniqueness }

    let max =
      let areality = Areality.Const.max in
      let linearity = Linearity.Const.max in
      let uniqueness = Uniqueness.Const.max in
      { areality; linearity; uniqueness }

    let le m0 m1 =
      Areality.Const.le m0.areality m1.areality
      && Uniqueness.Const.le m0.uniqueness m1.uniqueness
      && Linearity.Const.le m0.linearity m1.linearity

    let print ppf m = print () ppf (of_const m)

    let legacy =
      let areality = Areality.Const.legacy in
      let linearity = Linearity.Const.legacy in
      let uniqueness = Uniqueness.Const.legacy in
      { areality; linearity; uniqueness }

    let meet m0 m1 =
      let areality = Areality.Const.meet m0.areality m1.areality in
      let linearity = Linearity.Const.meet m0.linearity m1.linearity in
      let uniqueness = Uniqueness.Const.meet m0.uniqueness m1.uniqueness in
      { areality; linearity; uniqueness }

    let join m0 m1 =
      let areality = Areality.Const.join m0.areality m1.areality in
      let linearity = Linearity.Const.join m0.linearity m1.linearity in
      let uniqueness = Uniqueness.Const.join m0.uniqueness m1.uniqueness in
      { areality; linearity; uniqueness }

    (** See [Alloc.close_over] for explanation. *)
    let close_over m =
      let areality = m.areality in
      (* uniqueness of the returned function is not constrained *)
      let uniqueness = Uniqueness.Const.min in
      let linearity =
        Linearity.Const.join m.linearity
          (* In addition, unique argument make the returning function once.
             In other words, if argument <= unique, returning function >= once.
             That is, returning function >= (dual of argument) *)
          (C.unique_to_linear m.uniqueness)
      in
      { areality; linearity; uniqueness }

    (** See [Alloc.partial_apply] for explanation. *)
    let partial_apply m =
      let areality = m.areality in
      let uniqueness = Uniqueness.Const.min in
      let linearity = m.linearity in
      { areality; linearity; uniqueness }

    module Option = struct
      type some = t

      type t =
        ( Areality.Const.t option,
          Linearity.Const.t option,
          Uniqueness.Const.t option )
        modes

      let none = { areality = None; uniqueness = None; linearity = None }

      let value opt ~default =
        let areality = Option.value opt.areality ~default:default.areality in
        let uniqueness =
          Option.value opt.uniqueness ~default:default.uniqueness
        in
        let linearity = Option.value opt.linearity ~default:default.linearity in
        { areality; uniqueness; linearity }
    end
  end

  module List = struct
    type nonrec 'd t = 'd t list

    include Magic_allow_disallow (struct
      type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

      let allow_left l = List.map allow_left l

      let allow_right l = List.map allow_right l

      let disallow_left l = List.map disallow_left l

      let disallow_right l = List.map disallow_right l
    end)
  end

  let of_const = Const.of_const

  let zap_to_floor { comonadic; monadic } : Const.t =
    match Monadic.zap_to_floor monadic, Comonadic.zap_to_floor comonadic with
    | uniqueness, (areality, linearity) -> { areality; linearity; uniqueness }

  let zap_to_ceil { comonadic; monadic } : Const.t =
    match Monadic.zap_to_ceil monadic, Comonadic.zap_to_ceil comonadic with
    | uniqueness, (areality, linearity) -> { areality; linearity; uniqueness }

  let zap_to_legacy { comonadic; monadic } : Const.t =
    match Monadic.zap_to_legacy monadic, Comonadic.zap_to_legacy comonadic with
    | uniqueness, (areality, linearity) -> { areality; linearity; uniqueness }

  let check_const { comonadic; monadic } : Const.Option.t =
    let areality, linearity = Comonadic.check_const comonadic in
    let uniqueness = Monadic.check_const monadic in
    { areality; linearity; uniqueness }

  (** This is about partially applying [A -> B -> C] to [A] and getting [B ->
    C]. [comonadic] and [monadic] constutute the mode of [A], and we need to
    give the lower bound mode of [B -> C]. *)
  let close_over { comonadic; monadic } =
    (* If [A] is [local], [B -> C] containining a pointer to [A] must
       be [local] too. *)
    let areality = min_with_areality (Comonadic.areality comonadic) in
    (* [B -> C] is arrow type and thus crosses uniqueness *)
    (* If [A] is [once], [B -> C] containing a pointer to [A] must be [once] too
    *)
    let linearity0 = min_with_linearity (Comonadic.linearity comonadic) in
    (* Moreover, if [A] is [unique], [B -> C] must be [once]. *)
    let linearity1 =
      min_with_linearity (unique_to_linear (Monadic.uniqueness monadic))
    in
    join [areality; linearity0; linearity1]

  (** Similar to above, but we are given the mode of [A -> B -> C], and need to
      give the lower bound mode of [B -> C]. *)
  let partial_apply alloc_mode =
    (* [B -> C] should be always higher than [A -> B -> C] except the uniqueness
       axis where it's not constrained *)
    set_uniqueness_min alloc_mode
end

module Value = Mode (Regionality)
module Alloc = Mode (Locality)

let alloc_as_value m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Positive.via_monotone Value.Comonadic.Obj.obj
      (C.lift Product.SAxis0 C.Locality_as_regionality)
      comonadic
  in
  { comonadic; monadic }

let alloc_to_value_l2r m =
  let { comonadic; monadic } = Alloc.disallow_right m in
  let comonadic =
    S.Positive.via_monotone Value.Comonadic.Obj.obj
      (C.lift Product.SAxis0 C.Local_to_regional)
      comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2g : type l r. (l * r) Value.t -> (l * r) Alloc.t =
 fun m ->
  let { comonadic; monadic } = m in
  let comonadic =
    S.Positive.via_monotone Alloc.Comonadic.Obj.obj
      (C.lift Product.SAxis0 C.Regional_to_global)
      comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2l m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Positive.via_monotone Alloc.Comonadic.Obj.obj
      (C.lift Product.SAxis0 C.Regional_to_local)
      comonadic
  in
  { comonadic; monadic }
