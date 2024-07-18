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

    (* Monad Operations*)

    (** [return a] encapsulates elt array into Vector Type*)
    let return (a : elt array) = 
        if Array.length a = 0 then Empty
        else Vector a

    (** [bind v op] applies op to the underlying type of v*)
    let bind (v : 'a t) (op : elt array -> 'b t) = 
        match v with
        | Empty -> Empty
        | Vector a -> op a

    let ( >>= ) = bind
    (* -------------------------- *)

    let ( * ) = Element.mul
    let ( - ) = Element.sub
    let ( + ) = Element.add

    (* Redfine standard Array operations as operations on Vector*)
    let map f (v : 'a t) : 'a t = 
        v >>= fun a -> return (Array.map f a)

    let map2 f v1 v2 = 
        v1 >>= fun a1 -> 
        v2 >>= fun a2 ->
        return (Array.map2 f a1 a2)
    (* -------------------------- *)

    let init (size : int) (value : elt) = 
        if size < 0 then failwith "Negative array size"
        else if size = 0 then Empty
        else Vector(Array.make size value)

    let empty = Empty

    let is_empty = function
        | Empty -> true
        | Vector _ -> false

    let display = function 
        | Empty -> print_string "[]"
        | Vector v -> 
            Printf.printf "[ " ; 
            Array.iter (fun a -> Printf.printf "%s " (Element.to_string a)) v; 
            print_endline "]"

    (** [add v1 v2] is the element wise addition of v1 and v2
        Raises Invalid_argument if len v1 <> len v2 *)
    let add v1 v2 = map2 ( + ) v1 v2

    let sub v1 v2 = map2 ( - ) v1 v2

    let mul v s = map ( ( * ) s ) v

    (** [of_array arr] is the Vector containing the same elements as arr*)
    let of_array (arr : elt array) = return arr

    (** [of_list lst] is the Vector containing the same elements as lst*)
    let of_list (lst : elt list) = 
        if List.length lst = 0 then empty 
        else Vector(Array.of_list lst)

end
