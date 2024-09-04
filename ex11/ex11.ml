open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

(* let () = print_newline () *)
(* let u = FloatMatrix.of_array [|  *)
(*     [| 1.; -1.|]; *)
(*     [| -1.; 1.|]; *)
(* |] *)
(* let () = print_float (FloatMatrix.determinant u) *)
(**)
(* let () = print_newline () *)
(* let u = FloatMatrix.of_array [|  *)
(*     [| 2.; 0. ; 0.|]; *)
(*     [| 0.; 2. ; 0.|]; *)
(*     [| 0.; 0. ; 2.|]; *)
(* |] *)
(* let () = print_float (FloatMatrix.determinant u) *)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 8.; 5. ; -2.|];
    [| 4.; 7. ; 20.|];
    [| 7.; 6. ; 1.|];
|]
let () = print_float (FloatMatrix.determinant u)

let () = ignore(FloatMatrix.lup_decompo_ip u); FloatMatrix.display u

(* let () = print_newline () *)
(* let u = FloatMatrix.of_array [|  *)
(*     [| 8.; 5. ; -2.; 4.|]; *)
(*     [| 4.; 2.5; 20.; 4.|]; *)
(*     [| 8.; 5. ; 1.; 4.|]; *)
(*     [| 28.; -4. ; 17.; 1.|]; *)
(* |] *)
(* let () = print_float (FloatMatrix.determinant u) *)
(**)
(* let () = print_newline () *)
(* let u = FloatMatrix.of_array [|  *)
(*     [| 2.; 0.; 2.; 0.6 |]; *)
(*     [| 3.; 3.; 4.; -2.|]; *)
(*     [| 5.; 5.; 4.; 2.|]; *)
(*     [| -1.; -2.; 3.4; -1.|]; *)
(* |] *)
(* let () = print_float (FloatMatrix.determinant u) *)
