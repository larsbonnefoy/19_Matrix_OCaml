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
    (** elt is the type contained in Vector*)
    type elt 

    (** Is of type Vector or Empty*)
    type 'a t

    val init : int -> elt -> 'a t

    val empty : 'a t
    val display : 'a t -> unit
end

(* module Make (Ord : OrderedType) : S with type key = Ord.t *)
module Make (Element : Field) : S with type elt = Element.t
