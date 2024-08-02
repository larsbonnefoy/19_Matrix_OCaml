module type S = sig
    type elt 
    type t
    type v

    (** [make r c v] is a matrix with r rows and c columns with filled with value v*)
    val make : int -> int -> elt -> t

    (** [size m] is (row * col) *)
    val size : t -> int * int

    (** [is_empty m] is true if col or row m = 0*)
    val is_empty : t -> bool

    val to_string: t -> string
    val display : t -> unit
    val add : t -> t -> t
    val add_ip : t -> t -> unit
    val sub : t -> t -> t
    val sub_ip : t -> t -> unit
    val scl : t -> elt -> t
    val scl_ip : t -> elt -> unit
    val lerp : t -> t -> elt -> t
    val lerp_ip : t -> t -> elt -> unit
    val mul_vec : t -> v -> v
    val of_vector_array : v array -> t
    val of_array : elt array array -> t
end

module Make (Vector : Vector.S) : S with type elt = Vector.elt
