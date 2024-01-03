(* TEST
   * expect
*)

type t1 = [%external "t1"]
type t2 = [%external "t2"]
type floaty = [%external "%float"]

[%%expect{|
success
|}]

let f : t1 -> t2 = fun x -> x

[%%expect{|
failure
|}]

type t1' = [%external "t1"]

let f : t1 -> t1' = fun x -> x

[%%expect{|
failure
|}]

let f : floaty -> float = fun x -> x

[%%expect{|
failure
|}]

(* check that the compiler recognizes floaty as a float type by
   checking for the float-record optimization *)

external float_to_floaty : float -> floaty = "%identity"

type r = { x1 : float; x2 : floaty; x3 : float }

let tag = Obj.tag (Obj.repr { x1 = 3.14; x2 = float_to_floaty 5.68; x3 = 2.78 })

[%%expect{|
float_array_tag
|}]

(* similar, but for non-float *)

type r = { x1 : float; x2 : t1; x3 : float }

let tag = Obj.tag (Obj.repr { x1 = 3.14; x2 = Obj.magic (); x3 = 2.78 })

[%%expect{|
0
|}]

(* check that non-float external types skip the flat-float array optimization *)

let arr : t1 array = [| Obj.magic 3.14 |]
let tag = Obj.tag (Obj.repr arr)

[%%expect{|
not flat float array
|}]

(* This will be [type floaty2 = floaty = external "%float"] someday *)

type floaty2 = [%external ("%float" : floaty)]

let f : floaty -> floaty2 = fun x -> x

[%%expect{|
success
|}]

(* No private external types *)

type t4 = private [%external "boo"]

[%%expect{|
failure
|}]

(* No unboxed external types *)

type t5 = [%external "foo"] [@@unboxed]

[%%expect{|
failure
|}]

(* test module inclusion *)

module M : sig
  type t = [%external "foo"]
end = struct
  type t = [%external "foo"]
end

[%%expect{|
success
|}]

module M : sig
  type t
end = struct
  type t = [%external "foo"]
end

[%%expect{|
success
|}]

module M : sig
  type t = [%external "foo"]
end = struct
  type t
end

[%%expect{|
failure
|}]

module M : sig
  type t = [%external "foo"]
end = struct
  type t = [%external "bar"]
end

[%%expect{|
failure
|}]

module M : sig
  type t = [%external "%float"]
end = struct
  type t = [%external "%float"]
end

[%%expect{|
success
|}]

module M : sig
  type t
end = struct
  type t = [%external "%float"]
end

[%%expect{|
success
|}]

module M : sig
  type t = [%external "%int"]
end = struct
  type t = [%external "%float"]
end

[%%expect{|
failure
|}]

(* Cannot use "external" in a [with] constraint *)

module type S = sig
  type t
end

module type S2 = S with type t = [%external "%float"]

[%%expect{|
failure
|}]

(* Test variance and injectivity: all parameters are injective, but
   variance must be declared *)

type ('a, !'b, +'c, -'d, !+'e, !-'f) t = [%external "foo"]
type ('a, !'b, +'c, -'d, !+'e, !-'f) t2 = ('a, 'b, 'c, 'd, 'e, 'f) t
type 'a t3 = [%external "bar"]
type !'a t4 = 'a t3

[%%expect{|
success
|}]

type +'a t5 = 'a t3

[%%expect{|
failure
|}]

type -'a t6 = 'a t3

[%%expect{|
failure
|}]

(* check for injectivity propagation through recursive modules *)

module rec M1 : sig
  type 'a t = [%external "foo"]
end = struct
  type 'a t = [%external "foo"]
end

and M2 : sig
  type !'b t
end = struct
  type 'b t = 'b M1.t
end

[%%expect{|
success
|}]

(* check for manifests *)

type t8 = [%external "foo"]
type t9 = [%external ("foo" : t8)]

[%%expect{|
success
|}]

type t10 = [%external ("bar" : t8)]

[%%expect{|
failure
|}]

type t11 = [%external ("%float" : float)]

[%%expect{|
success
|}]

(* check for builtin *)

type bad = [%external "%bad"]

[%%expect{|
failure
|}]

(* check for separability *)

type 'a t = [%external "blah"]
type t2 = A : 'a t -> t2

[%%expect{|
failure
|}]

type 'a t = [%external "%int32"]
type t2 = A : 'a t -> t2

[%%expect{|
success
|}]

(* check for empty name *)

type t = [%external ""]

[%%expect{|
failure
|}]

type t = [%external 1 + 2]

[%%expect{|
failure
|}]
