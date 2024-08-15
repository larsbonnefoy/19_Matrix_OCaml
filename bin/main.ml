open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = Printexc.record_backtrace true

(* let notify_user f = *)
(*   try f () with e -> *)
(*     let msg = Printexc.to_string e *)
(*     and stack = Printexc.get_backtrace () in *)
(*       Printf.eprintf "there was an error: %s%s\n" msg stack; *)
(*       raise e *)

(* let u2 = notify_user (fun () -> FloatMatrix.of_array [| |]) *)
(* (* let v2 = FloatVector.of_array [| |] *) *)
(**)
(* let () = FloatMatrix.display u2 *)
(* let () = FloatVector.display v2 *)

let u = FloatVector.of_array [| 0.; 0.; 1.|]
let v = FloatVector.of_array [| 1.; 0.; 0.|]
let z = FloatVector.cross_product u v
let () = FloatVector.display z

let u = FloatVector.of_array [| 1.; 2.; 3.|]
let v = FloatVector.of_array [| 4.; 5.; 6.|]
let z = FloatVector.cross_product u v
let () = FloatVector.display z

let u = FloatVector.of_array [| 4.; 2.; -3.|]
let v = FloatVector.of_array [| -2.; -5.; 16.|]
let z = FloatVector.cross_product u v
let () = FloatVector.display z

let transpose_m = FloatMatrix.of_array [| 
    [|2.; 3.; 1.|];
    [|6.; 13.; 5.|];
    [|2.; 19.; 10.|];
|]

let () = FloatMatrix.display transpose_m
let () = print_newline () 
let () = FloatMatrix.display (FloatMatrix.transpose transpose_m)

let () = print_newline () 

let () = FloatMatrix.transpose_ip transpose_m 
let () = FloatMatrix.display transpose_m

let transpose_m = FloatMatrix.of_array [| 
    [|2.; 3.|];
    [|6.; 13.|];
    [|2.; 19.|];
|]

let () = FloatMatrix.display transpose_m
let () = print_newline () 
let () = FloatMatrix.display (FloatMatrix.transpose transpose_m)

let () = print_newline () 

let () = FloatMatrix.transpose_ip transpose_m 
let () = FloatMatrix.display transpose_m

let m_test = FloatMatrix.of_array [| 
    [|2.; 3.; 1.; 5.|];
    [|6.; 13.; 5.; 19.|];
    [|2.; 19.; 10.; 23.|];
    [|4.; 10.; 11.; 31.|];
|]

let (l, u) = FloatMatrix.lu_decompo m_test

let () = FloatMatrix.display l 

let () = print_newline ()

let () = FloatMatrix.display u 

let m_row_e = FloatMatrix.of_array [| 
    [|8.; 5.; -.2.; 4.; 28.|];
    [|4.; 2.5; 20.; 0.; -.4. |];
    [|8.; 5.; 1.; 4.; 17. |];
|]

let () = FloatMatrix.display m_row_e
let () = print_newline ()
let () = (FloatMatrix.switch_row 0 1 m_row_e) ; m_row_e |> FloatMatrix.display

(* let (l, u) = (FloatMatrix.lu_decompo m_row_e) *)
(**)
(* let () = print_newline () *)
(* let () = FloatMatrix.display u  *)
(* let () = print_newline () *)
(* let () = FloatMatrix.display l  *)
(* let () = print_newline () *)

let m_row_e = FloatMatrix.of_array [| 
    [|2.; 0.; 2.; 0.6|];
    [|3.; 3.; 4.; -2.|];
    [|5.; 5.; 4.; 2.|];
    [|-1.; -2.; 3.4; -1.|];
|]


let () = print_newline ()
let () = FloatMatrix.display m_row_e
let () = FloatMatrix.lup_decompo m_row_e
let () = print_newline ()
let () = FloatMatrix.display m_row_e


let det = FloatMatrix.of_array [| 
    [|8.; 5.; -2.; 4.|];
    [|4.; 2.5; 20.; 4.|];
    [|8.; 5.; 1.; 4.|];
    [|28.; -4.; 17.; -1.|];
|]
let () = FloatMatrix.lup_decompo det
let () = FloatMatrix.display det

let tr = FloatMatrix.of_array [| 
    [|8.; 5.; -2.|];
    [|4.; 2.5; 20.|];
    [|8.; 5.; 1.|];
    [|28.; -4.; 17.|];
|]
let () = print_float (FloatMatrix.trace tr)

let mul1 = FloatMatrix.of_array [| 
    [|1.; 2.; 3.|];
    [|4.; 5.; 6.|];
|]

let mul2 = FloatMatrix.of_array [| 
    [|7.; 8.|];
    [|9.; 10.|];
    [|11.; 12. |];
|]

let () = print_newline ()
let () = FloatMatrix.display mul1
let () = FloatMatrix.display mul2

let () = FloatMatrix.mul_mat mul1 mul2
