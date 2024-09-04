open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; -1.|];
    [| -1.; 1.|];
|]
let () = FloatMatrix.row_echelon_form_ip u; FloatMatrix.display u

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 0.; 0.|];
    [| 0.; 1.; 0.|];
    [| 0.; 0.; 1.|];
|]
let () = FloatMatrix.display (FloatMatrix.row_echelon_form u)
let () = FloatMatrix.row_echelon_form_ip u; FloatMatrix.display u


let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 2.|];
    [| 2.; 4.|];
|]
let () = FloatMatrix.display (FloatMatrix.row_echelon_form u)
let () = FloatMatrix.row_echelon_form_ip u; FloatMatrix.display u

let u = FloatMatrix.of_array [| 
    [| 8.; 5.; -2.; 4.; 28.|];
    [| 4.; 2.5; 20.; 4.; -4.|];
    [| 8.; 5.; 1.; 4.; 17.|];
|]
let () = FloatMatrix.display (FloatMatrix.row_echelon_form u)
let () = print_newline ()
let () = FloatMatrix.row_echelon_form_ip u; FloatMatrix.display u
