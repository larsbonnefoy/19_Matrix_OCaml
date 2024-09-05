open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 0.; 0.|];
    [| 0.; 1. ; 0.|];
    [| 0.; 0. ; 1.|];
|]
let () = FloatMatrix.display (FloatMatrix.inverse u)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 2.; 0.; 0.|];
    [| 0.; 2. ; 0.|];
    [| 0.; 0. ; 2.|];
|]
let () = FloatMatrix.display (FloatMatrix.inverse u)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 8.; 5. ; -2.|];
    [| 4.; 7. ; 20.|];
    [| 7.; 6. ; 1.|];
|]
let () = FloatMatrix.display (FloatMatrix.inverse u)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 8.; 5. ; -2.|];
    [| 4.; 7. ; 20.|];
    [| 4.; 7. ; 20.|];
|]
let () = FloatMatrix.display (FloatMatrix.inverse u)

