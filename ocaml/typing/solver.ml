(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Stephen Dolan, Jane Street, London                   *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Solver_intf

module Magic_allow_disallow (X : Allow_disallow) :
  Allow_disallow with type ('a, 'b, 'd) sided = ('a, 'b, 'd) X.sided = struct
  type ('a, 'b, 'd) sided = ('a, 'b, 'd) X.sided

  let disallow_right :
      type a b l r. (a, b, l * r) sided -> (a, b, l * disallowed) sided =
    Obj.magic

  let disallow_left :
      type a b l r. (a, b, l * r) sided -> (a, b, disallowed * r) sided =
    Obj.magic

  let allow_right :
      type a b l r. (a, b, l * allowed) sided -> (a, b, l * r) sided =
    Obj.magic

  let allow_left :
      type a b l r. (a, b, allowed * r) sided -> (a, b, l * r) sided =
    Obj.magic
end

type 'a error =
  { left : 'a;
    right : 'a
  }

(** Map the function to the list, and returns the first [Error] found;
    Returns [Ok] if no error, returning all the ['a] collected by combining
    the outputs of the passed function. In addition, all the ['x]s that produced
    [Ok None] are returned in a list. No guarantees about the orders of the
    lists. *)
let find_error (f : 'x -> ('a option, 'b) Result.t) (xs : 'x list) :
    ('a list * 'x list, 'b) Result.t =
  let rec loop a_acc x_acc = function
    | [] -> Ok (a_acc, x_acc)
    | x :: rest -> (
      match f x with
      | Ok None -> loop a_acc (x :: x_acc) rest
      | Ok (Some a) -> loop (a :: a_acc) x_acc rest
      | Error _ as e -> e)
  in
  loop [] [] xs

module Solver_mono (C : Lattices_mono) = struct
  type 'a var =
    { mutable vlower : 'a lmorphvar list;
          (** A list of variables directly under the current variable.
        Each is a pair [f] [v], and we have [f v <= u] where [u] is the current
        variable.
        TODO: consider using hashset for quicker deduplication *)
      mutable upper : 'a;  (** The precise upper bound of the variable *)
      mutable lower : 'a;
          (** The *conservative* lower bound of the variable.
       Why conservative: if a user calls [submode c u] where [c] is
        some constant and [u] some variable, we can modify [u.lower] of course.
       Idealy we should also modify all [v.lower] where [v] is variable above [u].
       However, we only have [vlower] not [vupper]. Therefore, the [lower] of
       higher variables are not updated immediately, hence conservative. Those
       [lower] of higher variables can be made precise later on demand, see
       [zap_to_floor_var_aux].

       One might argue for an additional [vupper] field, so that [lower] are
       always precise. While this might be doable, we note that the "hotspot" of
       the mode solver is to detect conflict, which is already achieved without
       precise [lower]. Adding [vupper] and keeping [lower] precise will come
       at extra cost. *)
      (* To summarize, INVARIANT:
         - For any variable [v], we have [v.lower <= v.upper].
         - Variables that have been fully constrained will have
         [v.lower = v.upper]. Note that adding a boolean field indicating that
         won't help much.
         - For any [v] and [f u \in v.vlower], we have [f u.upper <= v.upper], but not
         necessarily [f u.lower <= v.lower]. *)
      id : int  (** For identification/printing *)
    }

  and 'b lmorphvar = ('b, left_only) morphvar

  and ('b, 'd) morphvar =
    | Amorphvar : 'a var * ('a, 'b, 'd) C.morph -> ('b, 'd) morphvar
    | Asolvedvar : 'a -> ('a, 'd) morphvar

  let make_morphvar dst var morph =
    match C.is_constant_morph dst morph var.lower var.upper with
    | Some m -> Asolvedvar m
    | None -> Amorphvar (var, morph)

  module VarSet = Set.Make (Int)

  type change =
    | Cupper : 'a var * 'a -> change
    | Clower : 'a var * 'a -> change
    | Cvlower : 'a var * 'a lmorphvar list -> change

  type changes = change list

  let undo_change = function
    | Cupper (v, upper) -> v.upper <- upper
    | Clower (v, lower) -> v.lower <- lower
    | Cvlower (v, vlower) -> v.vlower <- vlower

  let undo_changes l = List.iter undo_change l

  (* To be filled in by [types.ml] *)
  let append_changes : (changes ref -> unit) ref = ref (fun _ -> assert false)

  let set_append_changes f = append_changes := f

  type ('a, 'd) mode =
    | Amode : 'a -> ('a, 'l * 'r) mode
    | Amodevar : { mutable mv : ('a, 'd) morphvar } -> ('a, 'd) mode
    | Amodejoin :
        { mutable m : 'a;
          mutable mvs : ('a, 'l * disallowed) morphvar list
        }
        -> ('a, 'l * disallowed) mode
        (** [Amodejoin a [mv0, mv1, ...]] represents [a join mv0 join mv1 join ...] *)
    | Amodemeet :
        { mutable m : 'a;
          mutable mvs : ('a, disallowed * 'r) morphvar list
        }
        -> ('a, disallowed * 'r) mode
        (** [Amodemeet a [mv0, mv1, ...]] represents [a meet mv0 meet mv1 meet ...]. *)

  (** Prints a mode variable, including the set of variables below it
      (recursively). To handle cycles, [traversed] is the set of variables that
      we have already printed and will be skipped. An example of cycle:

      Consider a lattice containing three elements A = {0, 1, 2} with the linear
      lattice structure: 0 < 1 < 2. Furthermore, we define a morphism
      f : A -> A
      f 0 = 0
      f 1 = 2
      f 2 = 2

      Note that f has a left right, which allows us to write f on the LHS of
      submode. Say we create a unconstrained variable [x], and invoke submode:
      f x <= x
      this would result in adding (f, x) into the [vlower] of [x]. That is,
      there will be a self-loop on [x].
      *)
  let rec print_var : type a. ?traversed:VarSet.t -> a C.obj -> _ -> a var -> _
      =
   fun ?traversed obj ppf v ->
    Format.fprintf ppf "%x[%a,%a]" v.id (C.print obj) v.lower (C.print obj)
      v.upper;
    match traversed with
    | None -> ()
    | Some traversed ->
      if VarSet.mem v.id traversed
      then ()
      else
        let traversed = VarSet.add v.id traversed in
        let p = print_morphvar ~traversed obj in
        Format.fprintf ppf "{%a}" (Format.pp_print_list p) v.vlower

  and print_morphvar :
      type a d. ?traversed:VarSet.t -> a C.obj -> _ -> (a, d) morphvar -> _ =
   fun ?traversed dst ppf -> function
    | Amorphvar (v, f) ->
      let src = C.src dst f in
      Format.fprintf ppf "%a(%a)" (C.print_morph dst) f
        (print_var ?traversed src) v
    | Asolvedvar m -> Format.fprintf ppf "solved:%a" (C.print dst) m

  let print_raw :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = true) (obj : a C.obj) ppf m ->
    let traversed = if verbose then Some VarSet.empty else None in
    match m with
    | Amode a -> C.print obj ppf a
    | Amodevar { mv } -> print_morphvar ?traversed obj ppf mv
    | Amodejoin { m; mvs } ->
      Format.fprintf ppf "join(%a,%a)" (C.print obj) m
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        mvs
    | Amodemeet { m; mvs } ->
      Format.fprintf ppf "meet(%a,%a)" (C.print obj) m
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        mvs

  include Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) mode constraint 'd = 'l * 'r

    let rec allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
      function
      | Amode c -> Amode c
      | Amodevar { mv } -> Amodevar { mv = allow_left_mv mv }
      | Amodejoin { m; mvs } ->
        Amodejoin { m; mvs = List.map allow_left_mv mvs }

    and allow_left_mv :
        type a l r. (a, allowed * r) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m) -> Amorphvar (v, C.allow_left m)
      | Asolvedvar m -> Asolvedvar m

    let rec allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
      function
      | Amode c -> Amode c
      | Amodevar { mv } -> Amodevar { mv = allow_right_mv mv }
      | Amodemeet { m; mvs } ->
        Amodemeet { m; mvs = List.map allow_right_mv mvs }

    and allow_right_mv :
        type a l r. (a, l * allowed) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m) -> Amorphvar (v, C.allow_right m)
      | Asolvedvar m -> Asolvedvar m

    let rec disallow_left :
        type a l r. (a, l * r) mode -> (a, disallowed * r) mode = function
      | Amode c -> Amode c
      | Amodevar { mv } -> Amodevar { mv = disallow_left_mv mv }
      | Amodejoin { m; mvs } ->
        Amodejoin { m; mvs = List.map disallow_left_mv mvs }
      | Amodemeet { m; mvs } ->
        Amodemeet { m; mvs = List.map disallow_left_mv mvs }

    and disallow_left_mv :
        type a l r. (a, l * r) morphvar -> (a, disallowed * r) morphvar =
      function
      | Amorphvar (v, m) -> Amorphvar (v, C.disallow_left m)
      | Asolvedvar m -> Asolvedvar m

    let rec disallow_right :
        type a l r. (a, l * r) mode -> (a, l * disallowed) mode = function
      | Amode c -> Amode c
      | Amodevar { mv } -> Amodevar { mv = disallow_right_mv mv }
      | Amodejoin { m; mvs } ->
        Amodejoin { m; mvs = List.map disallow_right_mv mvs }
      | Amodemeet { m; mvs } ->
        Amodemeet { m; mvs = List.map disallow_right_mv mvs }

    and disallow_right_mv :
        type a l r. (a, l * r) morphvar -> (a, l * disallowed) morphvar =
      function
      | Amorphvar (v, m) -> Amorphvar (v, C.disallow_right m)
      | Asolvedvar m -> Asolvedvar m
  end)

  let mlower dst = function
    | Amorphvar (var, morph) -> C.apply dst morph var.lower
    | Asolvedvar m -> m

  let mupper dst = function
    | Amorphvar (var, morph) -> C.apply dst morph var.upper
    | Asolvedvar m -> m

  let min (type a) (obj : a C.obj) = Amode (C.min obj)

  let max (type a) (obj : a C.obj) = Amode (C.max obj)

  let of_const a = Amode a

  let joins obj m ms = List.fold_left (C.join obj) m ms

  let meets obj m ms = List.fold_left (C.meet obj) m ms

  let apply_morphvar dst morph = function
    | Amorphvar (var, morph') ->
      make_morphvar dst var (C.compose dst morph morph')
    | Asolvedvar m -> Asolvedvar (C.apply dst morph m)

  let apply :
      type a b l r.
      b C.obj -> (a, b, l * r) C.morph -> (a, l * r) mode -> (b, l * r) mode =
   fun dst morph m ->
    let rec partition_map acc_mvs acc_solveds = function
      | [] -> acc_mvs, acc_solveds
      | mv :: mvs -> (
        match apply_morphvar dst morph mv with
        | Amorphvar _ as mv' -> partition_map (mv' :: acc_mvs) acc_solveds mvs
        | Asolvedvar m -> partition_map acc_mvs (m :: acc_solveds) mvs)
    in
    let partition_map mvs = partition_map [] [] mvs in
    match m with
    | Amode a -> Amode (C.apply dst morph a)
    | Amodevar { mv = Amorphvar (var, morph') } -> (
      match make_morphvar dst var (C.compose dst morph morph') with
      | Amorphvar _ as mv -> Amodevar { mv }
      | Asolvedvar m -> Amode m)
    | Amodevar { mv = Asolvedvar m } -> Amode (C.apply dst morph m)
    | Amodejoin { m; mvs } -> (
      let mvs', solveds = partition_map mvs in
      let const = joins dst (C.apply dst morph m) solveds in
      match mvs' with
      | [] -> Amode const
      | _ :: _ -> Amodejoin { m = const; mvs = mvs' })
    | Amodemeet { m; mvs } -> (
      let mvs', solveds = partition_map mvs in
      let const = meets dst (C.apply dst morph m) solveds in
      match mvs' with
      | [] -> Amode const
      | _ :: _ -> Amodemeet { m = const; mvs = mvs' })

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_lower ~log v a =
    (match log with
    | None -> ()
    | Some log -> log := Clower (v, v.lower) :: !log);
    v.lower <- a

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let update_lower (type a) ~log (obj : a C.obj) v a =
    set_lower ~log v (C.join obj v.lower a)

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_upper ~log v a =
    (match log with
    | None -> ()
    | Some log -> log := Cupper (v, v.upper) :: !log);
    v.upper <- a

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let update_upper (type a) ~log (obj : a C.obj) v a =
    set_upper ~log v (C.meet obj v.upper a)

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vlower ~log v vlower =
    (match log with
    | None -> ()
    | Some log -> log := Cvlower (v, v.vlower) :: !log);
    v.vlower <- vlower

  (* If changing the lower bound on the variable solves it, returns the
     resulting constant. *)
  let submode_cv :
      type a. log:_ -> a C.obj -> a -> a var -> (a option, a) Result.t =
    fun (type a) ~log (obj : a C.obj) a' v ->
     if C.le obj a' v.lower
     then Ok None
     else if not (C.le obj a' v.upper)
     then Error v.upper
     else (
       update_lower ~log obj v a';
       if C.le obj v.upper v.lower
       then (
         set_vlower ~log v [];
         Ok (Some v.upper))
       else Ok None)

  let submode_cmv :
      type a l.
      log:_ ->
      a C.obj ->
      a ->
      (a, l * allowed) morphvar ->
      (a option, a) Result.t =
   fun ~log obj a -> function
    | Amorphvar (v, f) as mv -> (
      (* Want a <= f v, therefore f' a <= v. Ideally the two are equivalent.
         However, [f v] could have been implicitly injected into a larger lattice,
         which means [a] could be outside of [f]'s co-domain, which is also [f']'s
         domain - so applying [f'] to [a] could be invalid.

         The following (seemingly redundant) several lines prevents that:
         - If a <= (f v).lower, immediately succeed
         - If not (a <= (f v).upper), immediately fail
         - Note that at this point, we still can't ensure that a >= (f v).lower.
           (We don't assume total ordering for generality.)
          Therefore, we set a = join a (f v).lower. This operation has no effect
          in terms of submoding, but we have now ensured that a is within the
          range of f v, and thus a is within the co-domain of f, and thus it's
          safe to apply f' to a. *)
      let mlower = mlower obj mv in
      let mupper = mupper obj mv in
      if C.le obj a mlower
      then Ok None
      else if not (C.le obj a mupper)
      then Error mupper
      else
        let a = C.join obj a mlower in
        let f' = C.left_adjoint obj f in
        let src = C.src obj f in
        let a' = C.apply src f' a in
        match submode_cv ~log src a' v with
        | Ok None -> Ok None
        | Ok (Some const) -> Ok (Some (C.apply obj f const))
        | Error _ -> assert false)
    | Asolvedvar m -> if C.le obj a m then Ok (Some m) else Error m

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
    (read: strictly higher) guess to replace the constant argument that MIGHT
    succeed. *)
  let rec submode_vc :
      type a. log:_ -> a C.obj -> a var -> a -> (a option, a) Result.t =
    fun (type a) ~log (obj : a C.obj) v a' ->
     if C.le obj v.upper a'
     then Ok None
     else if not (C.le obj v.lower a')
     then Error v.lower
     else (
       update_upper ~log obj v a';
       let r =
         v.vlower
         |> find_error (fun mu ->
                let r = submode_mvc ~log obj mu a' in
                (match r with
                | Ok None ->
                  (* Optimization: update [v.lower] based on [mlower u].*)
                  let mu_lower = mlower obj mu in
                  if not (C.le obj mu_lower v.lower)
                  then update_lower ~log obj v mu_lower
                | Ok (Some _) | Error _ -> ());
                r)
       in
       match r with
       | Ok (ms, mvs) ->
         set_lower ~log v (joins obj v.lower ms);
         if C.le obj v.upper v.lower
         then (
           set_vlower ~log v [];
           Ok (Some v.upper))
         else (
           set_vlower ~log v mvs;
           Ok None)
       | Error _ as e -> e)

  and submode_mvc :
        'a 'r.
        log:change list ref option ->
        'a C.obj ->
        ('a, allowed * 'r) morphvar ->
        'a ->
        ('a option, 'a) Result.t =
   fun ~log obj mv a ->
    match mv with
    | Amorphvar (v, f) as mv -> (
      (* See [submode_cmv] for why we need the following seemingly redundant
         lines. *)
      let mupper = mupper obj mv in
      let mlower = mlower obj mv in
      if C.le obj mupper a
      then Ok None
      else if not (C.le obj mlower a)
      then Error mlower
      else
        let a = C.meet obj a mupper in
        let f' = C.right_adjoint obj f in
        let src = C.src obj f in
        let a' = C.apply src f' a in
        (* If [mlower] was precise, then the check
           [not (C.le obj (mlower obj mv) a)] should guarantee the following call
           to return [Ok ()]. However, [mlower] is not precise *)
        (* not using [Result.map_error] to avoid allocating closure *)
        match submode_vc ~log src v a' with
        | Ok None -> Ok None
        | Ok (Some m) -> Ok (Some (C.apply obj f m))
        | Error e -> Error (C.apply obj f e))
    | Asolvedvar m -> if C.le obj m a then Ok (Some m) else Error m

  (** Zap the variable to its lower bound. Returns the [log] of the zapping, in
      case the caller are only interested in the lower bound and wants to
      reverse the zapping.

      As mentioned in [var], [v.lower] is not precise; to get the precise lower
      bound of [v], we call [submode v v.lower]. This would call [submode u
      v.lower] for every [u] in [v.vlower], which might fail because for some [u]
      [u.lower] is more up-to-date than [v.lower]. In that case, we call
      [submode v u.lower]. We repeat this process until no failure, and we will
      get the precise lower bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite.
      *)
  let zap_to_floor_var_aux (type a) (obj : a C.obj) (v : a var) =
    let rec loop lower =
      let log = ref [] in
      let r = submode_vc ~log:(Some log) obj v lower in
      match r with
      | Ok _ -> log, lower
      | Error a ->
        undo_changes !log;
        loop (C.join obj a lower)
    in
    loop v.lower

  let zap_to_floor_morphvar_aux dst = function
    | Amorphvar (v, f) ->
      let src = C.src dst f in
      let log, lower = zap_to_floor_var_aux src v in
      log, C.apply dst f lower
    | Asolvedvar m -> ref [], m

  let eq_morphvar :
      type a l0 r0 l1 r1.
      a C.obj -> (a, l0 * r0) morphvar -> (a, l1 * r1) morphvar -> bool =
   fun dst mv0 mv1 ->
    match mv0, mv1 with
    | Amorphvar (v0, f0), Amorphvar (v1, f1) -> (
      match C.eq_morph dst f0 f1 with None -> false | Some Refl -> v0 == v1)
    | Asolvedvar m0, Asolvedvar m1 -> C.le dst m0 m1 && C.le dst m1 m0
    | (Amorphvar _ | Asolvedvar _), _ -> false

  let exists obj mu mvs =
    if List.exists (fun mv -> eq_morphvar obj mv mu) mvs then true else false

  let submode_mvmv (type a) ~log (dst : a C.obj) mv mu =
    match mv, mu with
    | Amorphvar (v, f), Amorphvar (u, g) -> (
      if C.le dst (mupper dst mv) (mlower dst mu)
      then Ok (None, None)
      else if eq_morphvar dst mv mu
      then Ok (None, None)
      else
        (* The call f v <= g u translates to three steps:
           1. f v <= g u.upper
           2. f v.lower <= g u
           3. adding g' (f v) to the u.vlower, where g' is the left adjoint of g.
        *)
        match submode_mvc ~log dst mv (mupper dst mu) with
        | Error a -> Error (a, mupper dst mu)
        | Ok new_left -> (
          match submode_cmv ~log dst (mlower dst mv) mu with
          | Error a -> Error (mlower dst mv, a)
          | Ok new_right ->
            let g' = C.left_adjoint dst g in
            let src = C.src dst g in
            let g'f = C.compose src g' (C.disallow_right f) in
            let x = make_morphvar src v g'f in
            (match x with
            | Amorphvar _ ->
              if not (exists src x u.vlower)
              then set_vlower ~log u (x :: u.vlower)
            | Asolvedvar m -> update_lower ~log src u m);
            Ok (new_left, new_right)))
    | Asolvedvar m, Amorphvar _ -> (
      match submode_cmv ~log dst m mu with
      | Ok new_right -> Ok (Some m, new_right)
      | Error err_right -> Error (m, err_right))
    | Amorphvar _, Asolvedvar m -> (
      match submode_mvc ~log dst mv m with
      | Ok new_left -> Ok (new_left, Some m)
      | Error err_left -> Error (err_left, m))
    | Asolvedvar m0, Asolvedvar m1 ->
      if C.le dst m0 m1 then Ok (Some m0, Some m1) else Error (m0, m1)

  let cnt_id = ref 0

  let fresh ?upper ?lower ?vlower obj =
    let id = !cnt_id in
    cnt_id := id + 1;
    let upper = Option.value upper ~default:(C.max obj) in
    let lower = Option.value lower ~default:(C.min obj) in
    let vlower = Option.value vlower ~default:[] in
    { upper; lower; vlower; id }

  let submode_try (type a r l) (obj : a C.obj) (a : (a, allowed * r) mode)
      (b : (a, l * allowed) mode) =
    let log = Some (ref []) in
    let submode_cc left right =
      if C.le obj left right then Ok () else Error { left; right }
    in
    let submode_mvc v right =
      Result.map_error
        (fun left -> { left; right })
        (submode_mvc ~log obj v right)
    in
    let submode_cmv left v =
      Result.map_error
        (fun right -> { left; right })
        (submode_cmv ~log obj left v)
    in
    let submode_mvmv v u =
      Result.map_error
        (fun (left, right) -> { left; right })
        (submode_mvmv ~log obj v u)
    in
    let r =
      match a, b with
      | Amode left, Amode right -> submode_cc left right
      | Amodevar v, Amode right -> submode_mvc v right
      | Amode left, Amodevar v -> submode_cmv left v
      | Amodevar v, Amodevar u -> submode_mvmv v u
      | Amode a, Amodemeet (b, mvs) ->
        Result.bind (submode_cc a b) (fun () ->
            find_error (fun mv -> submode_cmv a mv) mvs)
      | Amodevar mv, Amodemeet (b, mvs) ->
        Result.bind (submode_mvc mv b) (fun () ->
            find_error (fun mv' -> submode_mvmv mv mv') mvs)
      | Amodejoin (a, mvs), Amode b ->
        Result.bind (submode_cc a b) (fun () ->
            find_error (fun mv' -> submode_mvc mv' b) mvs)
      | Amodejoin (a, mvs), Amodevar mv ->
        Result.bind (submode_cmv a mv) (fun () ->
            find_error (fun mv' -> submode_mvmv mv' mv) mvs)
      | Amodejoin (a, mvs), Amodemeet (b, mus) ->
        (* TODO: mabye create a intermediate variable? *)
        Result.bind (submode_cc a b) (fun () ->
            Result.bind
              (find_error (fun mv -> submode_mvc mv b) mvs)
              (fun () ->
                Result.bind
                  (find_error (fun mu -> submode_cmv a mu) mus)
                  (fun () ->
                    find_error
                      (fun mu -> find_error (fun mv -> submode_mvmv mv mu) mvs)
                      mus)))
    in
    match r with
    | Ok () -> Ok log
    | Error e ->
      (* we mutated some states and found conflict;
         need to revert those mutation to keep the state consistent.
         A nice by-effect is that this function doesn't mutate state in failure
      *)
      Option.iter (fun log -> undo_changes !log) log;
      Error e

  let submode obj a b =
    match submode_try obj a b with
    | Ok log ->
      Option.iter !append_changes log;
      Ok ()
    | Error _ as e -> e

  let zap_to_ceil_morphvar obj mv =
    assert (submode obj (Amode (mupper obj mv)) (Amodevar mv) |> Result.is_ok);
    mupper obj mv

  let zap_to_ceil : type a l. a C.obj -> (a, l * allowed) mode -> a =
   fun obj m ->
    match m with
    | Amode m -> m
    | Amodevar mv -> zap_to_ceil_morphvar obj mv
    | Amodemeet (a, mvs) ->
      List.fold_left
        (fun acc mv -> C.meet obj acc (zap_to_ceil_morphvar obj mv))
        a mvs

  let cons_dedup obj x xs = if exists obj x xs then xs else x :: xs

  (* Similar to [List.append] but dedup the result (assuming both inputs are
     deduped) *)
  let append_dedup obj l0 l1 =
    let rec loop acc rest =
      match rest with [] -> acc | x :: xs -> loop (cons_dedup obj x acc) xs
    in
    loop l0 l1

  let join (type a r) obj l =
    let rec loop :
        a ->
        (a, allowed * disallowed) morphvar list ->
        (a, allowed * r) mode list ->
        (a, allowed * disallowed) mode =
     fun a mvs rest ->
      if C.le obj (C.max obj) a
      then Amode (C.max obj)
      else
        match rest with
        | [] -> Amodejoin (a, mvs)
        | mv :: xs -> (
          match disallow_right mv with
          | Amode b -> loop (C.join obj a b) mvs xs
          (* some minor optimization: if [a] is lower than [mlower mv], we
              should keep the latter instead. This helps to fail early in
             [submode_try] *)
          | Amodevar mv ->
            loop (C.join obj a (mlower obj mv)) (cons_dedup obj mv mvs) xs
          | Amodejoin (b, mvs') ->
            loop (C.join obj a b) (append_dedup obj mvs' mvs) xs)
    in
    loop (C.min obj) [] l

  let meet (type a l) obj l =
    let rec loop :
        a ->
        (a, disallowed * allowed) morphvar list ->
        (a, l * allowed) mode list ->
        (a, disallowed * allowed) mode =
     fun a mvs rest ->
      if C.le obj a (C.min obj)
      then Amode (C.min obj)
      else
        match rest with
        | [] -> Amodemeet (a, mvs)
        | mv :: xs -> (
          match disallow_left mv with
          | Amode b -> loop (C.meet obj a b) mvs xs
          (* some minor optimization: if [a] is higher than [mupper mv], we
              should keep the latter instead. This helps to fail early in
             [submode_try] *)
          | Amodevar mv ->
            loop (C.meet obj a (mupper obj mv)) (cons_dedup obj mv mvs) xs
          | Amodemeet (b, mvs') ->
            loop (C.meet obj a b) (append_dedup obj mvs' mvs) xs)
    in
    loop (C.max obj) [] l

  let zap_to_floor_morphvar ~commit obj mv =
    let log, lower = zap_to_floor_morphvar_aux obj mv in
    if commit then !append_changes log else undo_changes !log;
    lower

  let zap_to_floor : type a r. a C.obj -> (a, allowed * r) mode -> a =
   fun obj m ->
    match m with
    | Amode a -> a
    | Amodevar mv -> zap_to_floor_morphvar ~commit:true obj mv
    | Amodejoin (a, mvs) ->
      List.fold_left
        (fun acc mv ->
          C.join obj acc (zap_to_floor_morphvar ~commit:true obj mv))
        a mvs

  let check_const : type a l r. a C.obj -> (a, l * r) mode -> a option =
   fun obj m ->
    match m with
    | Amode a -> Some a
    | Amodevar mv ->
      let lower = zap_to_floor_morphvar ~commit:false obj mv in
      if C.le obj (mupper obj mv) lower then Some lower else None
    | Amodemeet (a, mvs) ->
      let upper =
        List.fold_left (fun x mv -> C.meet obj x (mupper obj mv)) a mvs
      in
      let lower =
        List.fold_left
          (fun x mv ->
            C.meet obj x (zap_to_floor_morphvar ~commit:false obj mv))
          a mvs
      in
      if C.le obj upper lower then Some upper else None
    | Amodejoin (a, mvs) ->
      let upper =
        List.fold_left (fun x mv -> C.join obj x (mupper obj mv)) a mvs
      in
      let lower =
        List.fold_left
          (fun x mv ->
            C.join obj x (zap_to_floor_morphvar ~commit:false obj mv))
          a mvs
      in
      if C.le obj upper lower then Some lower else None

  let print :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = true) obj ppf m ->
    print_raw obj ~verbose ppf
      (match check_const obj m with None -> m | Some a -> Amode a)

  let newvar obj = Amodevar (Amorphvar (fresh obj, C.id))

  let newvar_above (type a r) (obj : a C.obj) (m : (a, allowed * r) mode) =
    match disallow_right m with
    | Amode a ->
      if C.le obj (C.max obj) a
      then Amode a, false
      else Amodevar (Amorphvar (fresh ~lower:a obj, C.id)), true
    | Amodevar mv ->
      (* [~lower] is not precise (because [mlower mv] is not precise), but
         it doesn't need to be *)
      ( Amodevar
          (Amorphvar (fresh ~lower:(mlower obj mv) ~vlower:[mv] obj, C.id)),
        true )
    | Amodejoin (a, mvs) ->
      (* [~lower] is not precise here, but it doesn't need to be *)
      Amodevar (Amorphvar (fresh ~lower:a ~vlower:mvs obj, C.id)), true

  let newvar_below (type a l) (obj : a C.obj) (m : (a, l * allowed) mode) =
    match disallow_left m with
    | Amode a ->
      if C.le obj a (C.min obj)
      then Amode a, false
      else Amodevar (Amorphvar (fresh ~upper:a obj, C.id)), true
    | Amodevar mv ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id) in
      let mu' = Amorphvar (u, C.id) in
      assert (Result.is_ok (submode_mvmv obj ~log:None mu mv));
      Amodevar mu', true
    | Amodemeet (a, mvs) ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id) in
      let mu' = Amorphvar (u, C.id) in
      assert (Result.is_ok (submode_mvc obj ~log:None mu a));
      List.iter
        (fun mv -> assert (Result.is_ok (submode_mvmv obj ~log:None mu mv)))
        mvs;
      Amodevar mu', true
end

module Solvers_polarized (C : Lattices_mono) = struct
  module S = Solver_mono (C)

  type changes = S.changes

  let undo_changes = S.undo_changes

  let set_append_changes = S.set_append_changes

  module type Solver_polarized =
    Solver_polarized
      with type ('a, 'b, 'd) morph := ('a, 'b, 'd) C.morph
       and type 'a obj := 'a C.obj
       and type 'a error := 'a error

  module rec Positive :
    (Solver_polarized
      with type 'd polarized = 'd pos
       and type ('a, 'd) mode_op = ('a, 'd) Negative.mode) = struct
    type 'd polarized = 'd pos

    type ('a, 'd) mode_op = ('a, 'd) Negative.mode

    type ('a, 'd) mode = ('a, 'd) S.mode constraint 'd = 'l * 'r

    include Magic_allow_disallow (S)

    let newvar = S.newvar

    let submode = S.submode

    let join = S.join

    let meet = S.meet

    let of_const _ = S.of_const

    let min = S.min

    let max = S.max

    let zap_to_floor = S.zap_to_floor

    let zap_to_ceil = S.zap_to_ceil

    let newvar_above = S.newvar_above

    let newvar_below = S.newvar_below

    let check_const = S.check_const

    let print ?(verbose = false) = S.print ~verbose

    let print_raw ?(verbose = false) = S.print_raw ~verbose

    let via_monotone = S.apply

    let via_antitone = S.apply
  end

  and Negative :
    (Solver_polarized
      with type 'd polarized = 'd neg
       and type ('a, 'd) mode_op = ('a, 'd) Positive.mode) = struct
    type 'd polarized = 'd neg

    type ('a, 'd) mode_op = ('a, 'd) Positive.mode

    type ('a, 'd) mode = ('a, 'r * 'l) S.mode constraint 'd = 'l * 'r

    include Magic_allow_disallow (struct
      type ('a, _, 'd) sided = ('a, 'd) mode

      let disallow_right = S.disallow_left

      let disallow_left = S.disallow_right

      let allow_right = S.allow_left

      let allow_left = S.allow_right
    end)

    let newvar = S.newvar

    let submode obj m0 m1 = S.submode obj m1 m0

    let join = S.meet

    let meet = S.join

    let of_const _ = S.of_const

    let min = S.max

    let max = S.min

    let zap_to_floor = S.zap_to_ceil

    let zap_to_ceil = S.zap_to_floor

    let newvar_above = S.newvar_below

    let newvar_below = S.newvar_above

    let check_const = S.check_const

    let print ?(verbose = false) = S.print ~verbose

    let print_raw ?(verbose = false) = S.print_raw ~verbose

    let via_monotone = S.apply

    let via_antitone = S.apply
  end

  (* Definitions to show that this solver works over a category. *)
  module Category = struct
    type 'a obj = 'a C.obj

    type ('a, 'b, 'd) morph = ('a, 'b, 'd) C.morph

    type ('a, 'd) mode =
      | Positive of ('a, 'd pos) Positive.mode
      | Negative of ('a, 'd neg) Negative.mode

    let apply_into_positive :
        type a b l r.
        b obj ->
        (a, b, l * r) morph ->
        (a, l * r) mode ->
        (b, l * r) Positive.mode =
     fun obj morph -> function
      | Positive mode -> Positive.via_monotone obj morph mode
      | Negative mode -> Positive.via_antitone obj morph mode

    let apply_into_negative :
        type a b l r.
        b obj ->
        (a, b, l * r) morph ->
        (a, l * r) mode ->
        (b, r * l) Negative.mode =
     fun obj morph -> function
      | Positive mode -> Negative.via_antitone obj morph mode
      | Negative mode -> Negative.via_monotone obj morph mode
  end
end
