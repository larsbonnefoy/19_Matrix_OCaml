(**Not really a field because to_string but whatever*)
module type Field = sig
    type t
    val zero : t
    val one : t
    val neg : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val to_string : t -> string
end

module type S = sig
    type elt 
    type 'a t
    val init : int -> elt -> 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val display : 'a t -> unit
    val add : 'a t -> 'a t -> 'a t
    val sub : 'a t -> 'a t -> 'a t
    val mul : 'a t -> elt -> 'a t
    val of_array : elt array -> 'a t
    val of_list : elt list -> 'a t
end

module Make(Element : Field) = struct

    type elt = Element.t

    type 'a t = Empty | Vector of elt array

    let init (size : int) (value : elt) = 
        if size < 0 then failwith "Negative array size"
        else if size = 0 then Empty
        else Vector(Array.make size value)

    let empty = Empty

    exception Empty_vector 

    let is_empty = function
        | Empty -> true
        | Vector _ -> false

    let display = function 
        | Empty -> print_string "[]"
        | Vector v -> 
            Printf.printf "[ " ; 
            Array.iter (fun a -> Printf.printf "%s " (Element.to_string a)) v; 
            print_endline "]"

    let value_helper = function
        | Empty -> raise Empty_vector
        | Vector v -> v

    (** [add v1 v2] is the element wise addition of v1 and v2
        Raises Invalid_argument if len v1 <> len v2
        Raises Empty_vector if v1 or v2 is empty*)
    let add v1 v2 = 
            let a = value_helper v1 in 
            let b = value_helper v2 in 
            Vector(Array.map2 Element.add a b)

    let sub v1 v2 = 
            let a = value_helper v1 in 
            let b = value_helper v2 in 
            Vector(Array.map2 Element.sub a b)

    let mul v s = 
        let a = value_helper v in
        let mul_by_scalar = Element.mul s in 
        Vector(Array.map mul_by_scalar a)

    (** [of_array arr] is the Vector containing the same elements as arr*)
    let of_array (arr : elt array) = 
        if Array.length arr = 0 then Empty 
        else Vector arr

    (** [of_list lst] is the Vector containing the same elements as lst*)
    let of_list (lst : elt list) = 
        if List.length lst = 0 then empty 
        else Vector(Array.of_list lst)
end
