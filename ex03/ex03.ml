open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)

let v1 = FloatVector.of_array [|0.; 0.|]
let v2 = FloatVector.of_array [|1.; 1.|]
let () = Printf.printf "\n%f\n" (FloatVector.dot v1 v2)
let () = Printf.printf "%f\n" (FloatVector.dot_fma v1 v2)

let v1 = FloatVector.of_array [|1.; 1.|]
let v2 = FloatVector.of_array [|1.; 1.|]
let () = Printf.printf "%f\n" (FloatVector.dot v1 v2)
let () = Printf.printf "%f\n" (FloatVector.dot_fma v1 v2)

let v1 = FloatVector.of_array [|-1.; 6.|]
let v2 = FloatVector.of_array [|3.; 2.|]
let () = Printf.printf "%f\n" (FloatVector.dot v1 v2)
let () = Printf.printf "%f\n" (FloatVector.dot_fma v1 v2)
