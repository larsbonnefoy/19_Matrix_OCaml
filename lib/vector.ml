module type Field = sig
    type t

    val zero : t

    val one : t

    val neg : t -> t

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val div : t -> t -> t
end

module type S = sig
    type elt 
    type 'a t
    val init : int -> elt -> 'a t
    val empty : 'a t
end

module Make(Element : Field) = struct

    type elt = Element.t

    type 'a t = Empty | Vector of elt array

    let init (size : int) (value : elt) = 
        if size < 0 then failwith ""
        else if size = 0 then Empty
        else Vector(Array.make size value)

    let empty = Empty

end
