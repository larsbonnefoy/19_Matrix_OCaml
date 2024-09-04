open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 0.|];
    [| 0.; 1.|];
|]
let () = FloatMatrix.display (FloatMatrix.transpose u)
let () = FloatMatrix.transpose_ip u; FloatMatrix.display u

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 2.; -5.; 0.|];
    [| 4.; 3.; 7.|];
    [| -2.; 3.; 4.|];
|]
let () = FloatMatrix.display (FloatMatrix.transpose u)
let () = print_newline ()
let () = FloatMatrix.transpose_ip u; FloatMatrix.display u

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 2.; -5.; 0.|];
    [| 4.; 3.; 7.|];
|]
let () = FloatMatrix.display (FloatMatrix.transpose u)
let () = print_newline ()
let () = FloatMatrix.transpose_ip u; FloatMatrix.display u
