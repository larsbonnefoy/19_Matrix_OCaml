open Linear_Algebra

module IntVector = Vector.Make(Int)
module FloatVector = Vector.Make(Float)

let vec1 = IntVector.init 2 1
let vec2 = FloatVector.init 2 0.

let () = IntVector.display vec1
let () = FloatVector.display vec2

let () = print_endline "EX 00"

let u = FloatVector.of_array [|2. ; 3.|]
let v = FloatVector.of_array [|5. ; 7.|]
let u_v = FloatVector.add u v
let () = FloatVector.display u_v

let u = FloatVector.of_array [|2. ; 3.|]
let v = FloatVector.of_array [|5. ; 7.|]
let u_v = FloatVector.sub u v
let () = FloatVector.display u_v

let u = FloatVector.of_array [|2. ; 3.|]
let s = 2.
let u_v = FloatVector.scl u s
let () = FloatVector.display u_v

let () = print_endline "EX 01"
let e1 = FloatVector.of_array [| 1.; 0.; 0. |]
let e2 = FloatVector.of_array [| 0.; 1.; 0. |]
let e3 = FloatVector.of_array [| 0.; 0.; 1. |]

let comb1 = FloatVector.linear_comb [| e1; e2; e3 |] [| 10.; ~-.2.; 0.5 |]
let () = FloatVector.display comb1

let v1 = FloatVector.of_array [| 1.; 2.; 3. |]
let v2 = FloatVector.of_array [| 0.; 10.; ~-.100. |]
let comb2 = FloatVector.linear_comb [| v1; v2 |] [| 10.; ~-.2.|]
let () = FloatVector.display comb2

