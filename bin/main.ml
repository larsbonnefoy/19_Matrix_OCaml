open Linear_Algebra

module IntVector = Vector.Make(Int)
module FloatVector = Vector.Make(Float)

let vec1 = IntVector.init 2 0
let vec2 = FloatVector.init 2 0.

let () = IntVector.display vec1
let () = FloatVector.display vec2
