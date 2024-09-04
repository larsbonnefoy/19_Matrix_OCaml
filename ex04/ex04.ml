open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)

let u = FloatVector.of_array [|0.; 0.; 0.|]
let () = Printf.printf "\n%f - %f - %f\n" (FloatVector.norm_1 u) (FloatVector.norm u) (FloatVector.norm_inf u)

let u = FloatVector.of_array [|1.; 2.; 3.|]
let () = Printf.printf "\n%f - %f - %f\n" (FloatVector.norm_1 u) (FloatVector.norm u) (FloatVector.norm_inf u)

let u = FloatVector.of_array [|-1.; -2.|]
let () = Printf.printf "\n%f - %f - %f\n" (FloatVector.norm_1 u) (FloatVector.norm u) (FloatVector.norm_inf u)
