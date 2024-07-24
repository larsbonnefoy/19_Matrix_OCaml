module type ReqOp = sig
    type t
    val zero : t
    val one : t
    val neg : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val fma : t -> t -> t -> t
    (* val abs : t -> t *)
    (* val sqrt : t -> float *)
    val to_string : t -> string
    (* val to_float : t -> float *)
end

module type S = sig
    (** elt is the type contained in Vector*)
    type elt 

    (** Is of type Matrix Vector array or Empty*)
    type t

    val init : int -> int -> elt -> t
    val is_empty : t -> bool
    val size : t -> int * int
    val display : t -> unit
    val to_string : t -> string
    val add : t -> t -> t
    (* val add_ip : t -> t -> unit *)
    val sub : t -> t -> t
    (* val sub_ip : t -> t -> unit *)
    val scl : t -> elt -> t
    (* val scl_ip : t -> elt -> unit *)
    (* val lerp : t -> t -> elt -> t *)
    val of_array : elt array array -> t
    (* val of_list : elt list list -> t *)
end

(* module Make (Ord : OrderedType) : S with type key = Ord.t *)
module Make (Element : ReqOp) : S with type elt = Element.t
