open LinearAlgebra

module FloatVector = Vector.Make(struct include Float let to_float (x : float) = x end)
module FloatMatrix = Matrix.Make(FloatVector)(Float)

let () = print_newline ()

let u = FloatVector.of_array [| 2.; 3.|]
let v = FloatVector.of_array [| 5.; 7.|]
let () = FloatVector.display (FloatVector.add u v)
let () = FloatVector.add_ip u v
let () = FloatVector.display u

let u = FloatVector.of_array [| 2.; 3.|]
let v = FloatVector.of_array [| 5.; 7.|]
let () = FloatVector.display (FloatVector.sub u v)
let () = FloatVector.sub_ip u v
let () = FloatVector.display u

let u = FloatVector.of_array [| 2.; 3.|]
let () = FloatVector.display (FloatVector.scl u 2.)
let () = FloatVector.scl_ip u 2.
let () = FloatVector.display u

let () = print_newline ( )
let u = FloatMatrix.of_array [| 
    [| 1.; 2.|];
    [| 3.; 4.|];
|]
let v = FloatMatrix.of_array [| 
    [| 7.; 4.|];
    [| -2.; 2.|];
|]
let () = FloatMatrix.display (FloatMatrix.add u v)
let () = FloatMatrix.add_ip u v
let () = FloatMatrix.display (u)

let () = print_newline ( )
let u = FloatMatrix.of_array [| 
    [| 1.; 2.|];
    [| 3.; 4.|];
|]
let v = FloatMatrix.of_array [| 
    [| 7.; 4.|];
    [| -2.; 2.|];
|]
let () = FloatMatrix.display (FloatMatrix.sub u v)
let () = FloatMatrix.sub_ip u v
let () = FloatMatrix.display (u)

let () = print_newline ( )
let u = FloatMatrix.of_array [| 
    [| 1.; 2.|];
    [| 3.; 4.|];
|]
let () = FloatMatrix.display (FloatMatrix.scl u 2.)
let () = FloatMatrix.scl_ip u 2.
let () = FloatMatrix.display (u)
