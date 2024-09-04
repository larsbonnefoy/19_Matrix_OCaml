open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 0.|];
    [| 0.; 1.|];
|]
let () = Printf.printf "%f\n" (FloatMatrix.trace u)

let u = FloatMatrix.of_array [| 
    [| 2.; -5.; 0.|];
    [| 4.; 3.; 7.|];
    [| -2.; 3.; 4.|];
|]
let () = Printf.printf "%f\n" (FloatMatrix.trace u)

let u = FloatMatrix.of_array [| 
    [| -2.; -8.; 4.|];
    [| 1.; -23.; 4.|];
    [| 0.; 6.; 4.|];
|]
let () = Printf.printf "%f\n" (FloatMatrix.trace u)
