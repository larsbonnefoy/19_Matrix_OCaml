open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)


let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 0. ; 0.|];
    [| 0.; 1. ; 0.|];
    [| 0.; 0. ; 1.|];
|]
let () = print_int (FloatMatrix.rank u)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 1.; 2. ; 0.; 0.|];
    [| 2.; 4. ; 0.; 0.|];
    [| -1.; 2. ; 1.; 1.|];
|]
let () = print_int (FloatMatrix.rank u)

let () = print_newline ()
let u = FloatMatrix.of_array [| 
    [| 8.; 5. ; -2.|];
    [| 4.; 7. ; 20.|];
    [| 7.; 6. ; 1.|];
    [| 21.; 18. ; 7.|];
|]
let () = print_int (FloatMatrix.rank u)
