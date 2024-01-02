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

