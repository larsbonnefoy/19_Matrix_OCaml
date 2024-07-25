open Linear_Algebra

module IntVector = Vector.Make(struct include Int 
                                let fma x y z = x * y + z 
                                let sqrt x = Float.sqrt (float_of_int x)
                                end)

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)


module FloatMatrix = Matrix.Make(Float)

let () = print_newline () ; print_endline "=====EX 00===="

let u = FloatVector.of_array [|2. ; 3.|]
let v = FloatVector.of_array [|5. ; 7.|]
let u_v = FloatVector.add u v
let () = FloatVector.display u_v

let () = FloatVector.add_ip u v
let () = print_string "Add inplace " ; FloatVector.display u

let u = FloatVector.of_array [|2. ; 3.|]
let v = FloatVector.of_array [|5. ; 7.|]
let u_v = FloatVector.sub u v
let () = FloatVector.display u_v

let () = FloatVector.sub_ip u v
let () = print_string "sub inplace " ; FloatVector.display u

let u = FloatVector.of_array [|2. ; 3.|]
let s = 2.
let u_s = FloatVector.scl u s
let () = print_string "u * s: " ; FloatVector.display u_s
let () = print_string "Not changed: " ; FloatVector.display u

let () = FloatVector.scl_ip u s
let () = print_string "u changed ip: " ; FloatVector.display u

let u = FloatMatrix.of_array [| [|1.; 2.|]; 
                                [|3.; 4.|];|]

let v = FloatMatrix.of_array [| [|7.; 4.|]; 
                                [|-2.; 2.|];|]

let () = FloatMatrix.display (FloatMatrix.add u v)
let () = print_newline ()
let () = FloatMatrix.display (FloatMatrix.sub u v)
let () = print_newline ()
let () = FloatMatrix.display (FloatMatrix.scl u 2.)
let () = FloatMatrix.scl_ip u 2.
let () = FloatMatrix.display u
let () = FloatMatrix.display v

let u = FloatMatrix.of_array [| [|1.; 2.|]; 
                                [|3.; 4.|];
                                [|3.; 4.|];|]

let v = FloatMatrix.of_array [| [|7.; 4.|]; 
                                [|-2.; 2.|];
                                [|-2.; 2.|];|]
let () = print_newline ()
let () = FloatMatrix.add_ip u v 
let () = FloatMatrix.display u

let () = print_endline "====EX 01===="
let e1 = FloatVector.of_array [| 1.; 0.; 0. |]
let e2 = FloatVector.of_array [| 0.; 1.; 0. |]
let e3 = FloatVector.of_array [| 0.; 0.; 1. |]

let comb1 = FloatVector.linear_comb [| e1; e2; e3 |] [| 10.; ~-.2.; 0.5 |]
let () = FloatVector.display comb1

let v1 = FloatVector.of_array [| 1.; 2.; 3. |]
let v2 = FloatVector.of_array [| 0.; 10.; ~-.100. |]
let comb2 = FloatVector.linear_comb [| v1; v2 |] [| 10.; ~-.2.|]
let () = FloatVector.display comb2
let comb3 = FloatVector.linear_comb_fma [| v1; v2 |] [| 10.; ~-.2.|]
let () = FloatVector.display comb3

let () = print_endline "====EX 02===="
let () = Printf.printf "%.2f %.2f %.2f %.2f\n" (FloatVector.lerp_p 0. 1. 0.) (FloatVector.lerp_p 0. 1. 1.) (FloatVector.lerp_p 0. 1. 0.) (FloatVector.lerp_p 21. 42. 0.3)

let u = FloatMatrix.of_array [| [|2.; 1.|]; [| 3.; 4.|]|]
let v = FloatMatrix.of_array [| [|20.; 10.|]; [| 30.; 40.|]|]

let () = FloatMatrix.display (FloatMatrix.lerp u v 0.5)

let () = print_endline "====EX 03===="
let u = FloatVector.of_array [|0.; 0.|]
let v = FloatVector.of_array [|1.; 1.|]
let () = Printf.printf "%f %f\n" (FloatVector.dot u v) (FloatVector.dot_fma u v)

let u = FloatVector.of_array [|1.; 1.|]
let v = FloatVector.of_array [|1.; 1.|]
let () = Printf.printf "%f %f\n" (FloatVector.dot u v) (FloatVector.dot_fma u v)

let u = FloatVector.of_array [|-1.; 6.|]
let v = FloatVector.of_array [|3.; 2.|]
let () = Printf.printf "%f %f\n" (FloatVector.dot u v) (FloatVector.dot_fma u v)
