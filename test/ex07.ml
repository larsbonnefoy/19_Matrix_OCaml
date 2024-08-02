open LinearAlgebra
open OUnit2

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)

let u1 = FloatMatrix.of_array [| 
    [|1.|];
    [|1.|];
|]
let v1 = FloatVector.of_array [| 4.; 2.|]
let test1 = fun () -> FloatMatrix.mul_vec u1 v1

let u2 = FloatMatrix.of_array [| 
    [| 1.; 1.|]
|]
let v2 = FloatVector.of_array [| 1.; |]
let test2 = fun () -> FloatMatrix.mul_vec u2 v2

let u = FloatMatrix.of_array [| 
    [|1.; 0.|];
    [|0.; 1.|]
|]
let v = FloatVector.of_array [| 4.; 2.|]
let test3 = ((FloatMatrix.mul_vec u v) = FloatVector.of_array [| 4.; 2.|])

let u = FloatMatrix.of_array [| 
    [|2.; 5.|];
    [|1.; 3.|]
|]
let v = FloatVector.of_array [| 1.; 2.|]
let v' = FloatMatrix.mul_vec u v
let () = FloatMatrix.mul_vec_ip u v
let test4 = v' = v

let tests = "Tests for ex07" >::: [
    "test1: Different lengths" >:: (fun _ -> assert_raises (Invalid_argument "mul_vec: dim of m (2, 1) do not match size of v (2)") test1);
    "test2: Different lengths" >:: (fun _ -> assert_raises (Invalid_argument "mul_vec: dim of m (1, 2) do not match size of v (1)") test2);
    "test3: Check vector matrix mult" >:: (fun _ -> assert_bool "" test3);
    "test3: Check ip mult = mult" >:: (fun _ -> assert_bool "" test4)
 ]

let _ = run_test_tt_main tests

let u = FloatMatrix.of_array [| 
    [|1.; 0.|];
    [|0.; 1.|]
|]

let v = FloatVector.of_array [| 4.; 2.|]

let () = FloatVector.display (FloatMatrix.mul_vec u v)

let u = FloatMatrix.of_array [| 
    [|2.; 5.|];
    [|1.; 3.|]
|]

let v = FloatVector.of_array [| 1.; 2.|]

let v' = (FloatMatrix.mul_vec u v)

let () = FloatVector.display v'

let () = FloatMatrix.mul_vec_ip u v

let () = FloatVector.display v
