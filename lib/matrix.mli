module type S = sig
    type elt 
    type t
    type v

    (** [make r c v] is a matrix with r rows and c columns with filled with value v*)
    val make : int -> int -> elt -> t

    val init : int -> int -> (int -> int -> elt) -> t

    (** [size m] is (row * col) *)
    val size : t -> int * int

    (** [is_empty m] is true if col or row m = 0*)
    val is_empty : t -> bool

    val is_square : t -> bool

    val id : int -> int -> t

    val switch_row: int -> int -> t -> unit

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
    val mul_vec_ip : t -> v -> unit
    val mul_mat : t -> t -> t
    val mul_mat_ip : t -> t -> unit
    val lup_decompo : t -> t * int array
    val lup_decompo_ip : t -> int array
    val trace : t -> elt
    val transpose : t -> t
    val transpose_ip : t -> unit
    val row_echelon_form: t -> t
    val row_echelon_form_ip: t -> unit
    val determinant: t -> elt
    val inverse: t -> t
    val rank: t -> int
    val of_vector_array : v array -> t
    val of_array : elt array array -> t
end

module type EltOp = sig
    type t
    val zero : t
    val one : t
    val neg : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val to_string : t -> string
    val abs : t -> t
    val fma : t -> t -> t -> t
end

module Make (Vector : Vector.S) (Element : EltOp with type t = Vector.elt) : S with type elt = Element.t and type v = Vector.t
