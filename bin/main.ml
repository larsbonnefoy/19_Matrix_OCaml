open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)

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

let () = print_newline () 

let m_test = FloatMatrix.init 10 10 (fun r c -> if r < c then 1. else 0.)

let () = FloatMatrix.display m_test
