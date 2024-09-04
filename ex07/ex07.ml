open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = print_newline ()

let u = FloatMatrix.of_array [| 
    [| 1.; 0.|];
    [| 0.; 1.|];
|]
let v = FloatVector.of_array [| 4.; 2.|]
let () = FloatVector.display (FloatMatrix.mul_vec u v)
let () = FloatMatrix.mul_vec_ip u v; FloatVector.display v

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 2.; 0.|];
    [| 0.; 2.|];
|]
let v = FloatVector.of_array [| 4.; 2.|]
let () = FloatVector.display (FloatMatrix.mul_vec u v)
let () = FloatMatrix.mul_vec_ip u v; FloatVector.display v

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 2.; -2.|];
    [| -2.; 2.|];
|]
let v = FloatVector.of_array [| 4.; 2.|]
let () = FloatVector.display (FloatMatrix.mul_vec u v)
let () = FloatMatrix.mul_vec_ip u v; FloatVector.display v

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 0.|];
    [| 0.; 1.|];
|]
let v = FloatMatrix.of_array [| 
    [| 1.; 0.|];
    [| 0.; 1.|];
|]
let () = FloatMatrix.display (FloatMatrix.mul_mat u v)
let () = FloatMatrix.mul_mat_ip u v; FloatMatrix.display u 

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 0.|];
    [| 0.; 1.|];
|]
let v = FloatMatrix.of_array [| 
    [| 2.; 1.|];
    [| 4.; 2.|];
|]
let () = FloatMatrix.display (FloatMatrix.mul_mat u v)
let () = FloatMatrix.mul_mat_ip u v; FloatMatrix.display u 

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 3.; -5.|];
    [| 6.; 8.|];
|]
let v = FloatMatrix.of_array [| 
    [| 2.; 1.|];
    [| 4.; 2.|];
|]
let () = FloatMatrix.display (FloatMatrix.mul_mat u v)
let () = FloatMatrix.mul_mat_ip u v; FloatMatrix.display u 
