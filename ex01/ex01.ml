open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = print_newline ()

let e1 = FloatVector.of_array [| 1.; 0.; 0.|]
let e2 = FloatVector.of_array [| 0.; 1.; 0.|]
let e3 = FloatVector.of_array [| 0.; 0.; 1.|]

let v1 = FloatVector.of_array [| 1.; 2.; 3.|]
let v2 = FloatVector.of_array [| 0.; 10.; -100.|]

let res = FloatVector.linear_comb_fma [| e1; e2; e3 |] [| 10.; -2.; 0.5|]
let () = FloatVector.display res

let res = FloatVector.linear_comb_fma [| v1; v2 |] [| 10.; -2.|]
let () = FloatVector.display res
