open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)

let () = print_newline ()

let u = FloatVector.of_array [|0.; 0.; 1.|]
let v = FloatVector.of_array [|1.; 0.; 0.|]
let () = FloatVector.display (FloatVector.cross_product u v)
let () = (FloatVector.cross_product_ip u v) ; FloatVector.display u

let u = FloatVector.of_array [|1.; 2.; 3.|]
let v = FloatVector.of_array [|4.; 5.; 6.|]
let () = FloatVector.display (FloatVector.cross_product u v)
let () = (FloatVector.cross_product_ip u v) ; FloatVector.display u

let u = FloatVector.of_array [|4.; 2.; -3.|]
let v = FloatVector.of_array [|-2.; -5.; 16.|]
let () = FloatVector.display (FloatVector.cross_product u v)
let () = (FloatVector.cross_product_ip u v) ; FloatVector.display u
