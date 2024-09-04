open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)

let u = FloatVector.of_array [|1.; 0.|]
let v = FloatVector.of_array [|1.; 0.|]
let () = Printf.printf "\n%f\n" (FloatVector.cos u v)

let u = FloatVector.of_array [|1.; 0.|]
let v = FloatVector.of_array [|0.; 1.|]
let () = Printf.printf "%f\n" (FloatVector.cos u v)

let u = FloatVector.of_array [|-1.; 1.|]
let v = FloatVector.of_array [|1.; -1.|]
let () = Printf.printf "%f\n" (FloatVector.cos u v)

let u = FloatVector.of_array [|2.; 1.|]
let v = FloatVector.of_array [|4.; 2.|]
let () = Printf.printf "%f\n" (FloatVector.cos u v)

let u = FloatVector.of_array [|1.; 2.; 3.|]
let v = FloatVector.of_array [|4.; 5.; 6.|]
let () = Printf.printf "%f\n" (FloatVector.cos u v)
