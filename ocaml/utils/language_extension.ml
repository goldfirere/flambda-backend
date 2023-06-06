module Maturity = struct
  type t = Stable | Beta | Alpha

  let compare t1 t2 =
    let rank = function
      | Stable -> 1
      | Beta -> 2
      | Alpha -> 3
    in
    compare (rank t1) (rank t2)
end

module M = struct
  type _ t =
    | Comprehensions : unit t
    | Local : unit t
    | Include_functor : unit t
    | Polymorphic_parameters : unit t
    | Immutable_arrays : unit t
    | Module_strengthening : unit t
    | Layouts : Maturity.t t
end
include M

let get_ordered : type a. a t -> (module Misc.OrderedType with type t = a) =
  function
  | Comprehensions -> (module Unit)
  | Local -> (module Unit)
  | Include_functor -> (module Unit)
  | Polymorphic_parameters -> (module Unit)
  | Immutable_arrays -> (module Unit)
  | Module_strengthening -> (module Unit)
  | Layouts -> (module Maturity)

let to_string : type a. a t -> string = function
  | Comprehensions -> "comprehensions"
  | Local -> "local"
  | Include_functor -> "include_functor"
  | Polymorphic_parameters -> "polymorphic_parameters"
  | Immutable_arrays -> "immutable_arrays"
  | Module_strengthening -> "module_strengthening"
  | Layouts -> "layouts"

module Exist = struct
  include Misc.Exist(M)

  let to_string : t -> string = function
    | Pack ext -> to_string ext
end

let equal_t (type a b) (a : a t) (b : b t) : (a, b) Misc.eq option = match a, b with
  | Comprehensions, Comprehensions -> Some Refl
  | Local, Local -> Some Refl
  | Include_functor, Include_functor -> Some Refl
  | Polymorphic_parameters, Polymorphic_parameters -> Some Refl
  | Immutable_arrays, Immutable_arrays -> Some Refl
  | Module_strengthening, Module_strengthening -> Some Refl
  | Layouts, Layouts -> Some Refl
  | (Comprehensions | Local | Include_functor | Polymorphic_parameters |
     Immutable_arrays | Module_strengthening | Layouts), _ -> None

let equal a b = Option.is_some (equal_t a b)

type ext_pair = Misc.Pair1(M)(Misc.T1.Id).t
let default_extensions : ext_pair list =
  [ Pair (Local, ())
  ; Pair (Include_functor, ())
  ; Pair (Polymorphic_parameters, ())
  ]

let of_string extn : Exist.t option = match String.lowercase_ascii extn with
  | "comprehensions" -> Some (Pack Comprehensions)
  | "local" -> Some (Pack Local)
  | "include_functor" -> Some (Pack Include_functor)
  | "polymorphic_parameters" -> Some (Pack Polymorphic_parameters)
  | "immutable_arrays" -> Some (Pack Immutable_arrays)
  | "strengthening" -> Some (Pack Module_strengthening)
  | "layouts" -> Some (Pack Layouts)
  | _ -> None

let of_string_exn extn =
  match of_string extn with
  | Some extn -> extn
  | None -> raise (Arg.Bad(Printf.sprintf "Extension %s is not known" extn))

(* We'll do this in a more principled way later. *)
(* CR layouts: Note that layouts is only "mostly" erasable, because of annoying
   interactions with the pre-layouts [@@immediate] attribute like:

     type ('a : immediate) t = 'a [@@immediate]

   But we've decided to punt on this issue in the short term.
*)
let is_erasable : type a. a t -> bool = function
  | Local
  | Layouts ->
      true
  | Comprehensions
  | Include_functor
  | Polymorphic_parameters
  | Immutable_arrays
  | Module_strengthening ->
      false

module Universe = struct
  (** Which extensions can be enabled? *)
  type t =
    | No_extensions
    | Only_erasable
    | Any

  let compiler_options = function
    | No_extensions -> "flag -disable-all-extensions"
    | Only_erasable -> "flag -only-erasable-extensions"
    | Any           -> "default options"

  let is_allowed t ext = match t with
    | No_extensions -> false
    | Only_erasable -> is_erasable ext
    | Any           -> true
end

(* Mutable state.  Invariants:

   (1) [!extensions] contains at most one copy of each extension.

   (2) Every member of [!extensions] satisfies [Universe.is_allowed !universe].
       (For instance, [!universe = No_extensions] implies
       [!extensions = []]). *)
let extensions = ref default_extensions (* -extension *)
let universe   = ref Universe.Any       (* -only-erasable-extensions,
                                           -disable-all-extensions *)

let set (type a) (extn : a t) = function
  | Some value ->
    if not (Universe.is_allowed !universe extn) then
      raise (Arg.Bad(Printf.sprintf
        "Cannot enable extension %s: incompatible with %s"
        (to_string extn)
        (Universe.compiler_options !universe)));
    let module Ops = Misc.OrderedTypeOps (val get_ordered extn) in
    let rec update_extensions already_seen : ext_pair list -> ext_pair list =
      function
      | [] -> Pair (extn, value) :: already_seen
      | ((Pair (extn', v) as e) :: es) ->
         match equal_t extn extn' with
         | None -> update_extensions (e :: already_seen) es
         | Some Refl ->
            Pair (extn, Ops.max v value) :: List.rev_append already_seen es
    in
    extensions := update_extensions [] !extensions
  | None ->
    extensions :=
      List.filter (fun (Pair (extn', _) : ext_pair) -> not (equal extn extn'))
        !extensions

let enable extn value = set extn (Some value)
let disable extn = set extn None

let is_at_least (type a) (extn : a t) (value : a) =
  let rec check : ext_pair list -> bool = function
    | [] -> false
    | (Pair (e, v) :: es) -> match equal_t e extn with
                             | Some Refl -> compare v value >= 0
                             | None -> check es
  in
  check !extensions

let is_enabled extn = is_at_least extn ()

(* It might make sense to ban [set], [enable], [disable],
   [only_erasable_extensions], and [disallow_extensions] inside [f], but it's
   not clear that it's worth the hassle *)
let with_set extn value f =
  (* This is similar to [Misc.protect_refs], but we don't have values to set
     [extensions] and [universe] to. *)
  let current_extensions = !extensions in
  let current_universe   = !universe   in
  Fun.protect
    ~finally:(fun () ->
      extensions := current_extensions;
      universe   := current_universe)
    (fun () ->
       set extn value;
       f ())

let with_enabled extn value = with_set extn (Some value)
let with_disabled extn = with_set extn None

let restrict_to_erasable_extensions () =
  match !universe with
  | Any ->
     extensions :=
       List.filter (fun (Pair (extn, _) : ext_pair) -> is_erasable extn)
         !extensions;
      universe   := Universe.Only_erasable
  | Only_erasable ->
      () (* Idempotent *)
  | No_extensions ->
      raise (Arg.Bad(Printf.sprintf
        "Cannot specify %s: incompatible with %s"
        (Universe.compiler_options Only_erasable)
        (Universe.compiler_options No_extensions)))

let disable_all () =
  disable Comprehensions;
  disable Local;
  disable Include_functor;
  disable Polymorphic_parameters;
  disable Immutable_arrays;
  disable Module_strengthening;
  disable Layouts

let disallow_extensions () =
  (* The strictest option, so no internal checks needed *)
  extensions := [];
  universe   := Universe.No_extensions

let enable_maximal () =
  enable Comprehensions ();
  enable Local ();
  enable Include_functor ();
  enable Polymorphic_parameters ();
  enable Immutable_arrays ();
  enable Module_strengthening ();
  enable Layouts Alpha
