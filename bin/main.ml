open Linear_Algebra

module IntVector = Vector.Make(Int)
module FloatVector = Vector.Make(Float)

let vec1 = IntVector.init 2 1
let vec2 = FloatVector.init 2 0.

let () = IntVector.display vec1
let () = FloatVector.display vec2

let () = print_endline "EX 00"

let u = FloatVector.of_array [|2. ; 3.|]

let v = FloatVector.of_array [|5. ; 7.|]

let u_v = FloatVector.add u v

let () = FloatVector.display u_v
