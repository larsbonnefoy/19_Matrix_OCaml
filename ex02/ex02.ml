open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = Printf.printf "%f\n" (FloatVector.lerp_p 0. 1. 1.)
let () = Printf.printf "%f\n" (FloatVector.lerp_p 0. 1. 1.)
let () = Printf.printf "%f\n" (FloatVector.lerp_p 0. 1. 0.5)
let () = Printf.printf "%f\n" (FloatVector.lerp_p 21. 42. 0.3)

let v1 = FloatVector.of_array [|2.; 1.|]
let v2 = FloatVector.of_array [|4.; 2.|]
let () = FloatVector.display (FloatVector.lerp v1 v2 0.3)
let () = FloatVector.lerp_ip v1 v2 0.3 ; FloatVector.display v1

let m1 = FloatMatrix.of_array [|[|2.; 1.|] ; [|3.; 4.|]|]
let m2 = FloatMatrix.of_array [|[|20.; 10.|] ; [|30.; 40.|]|]
let () = FloatMatrix.display (FloatMatrix.lerp m1 m2 0.5)
let () = (FloatMatrix.lerp_ip m1 m2 0.5); FloatMatrix.display m1
