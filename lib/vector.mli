module type Field = sig
    type t
    val zero : t
    val one : t
    val neg : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t

    (** Defined only in Floats, required for other implementation
        fma x y z returns x * y + z *)
    val fma : t -> t -> t -> t
    val to_string : t -> string
end

module type S = sig
    (** elt is the type contained in Vector*)
    type elt 

    (** Is of type Vector 'a or Empty*)
    type t

    val init : int -> elt -> t

    val empty : t

    (** [is_empty v] is true if v is Empty, false if v is Vector _*)
    val is_empty : t -> bool

    (** [length v] the number of elements contained in v*)
    val length : t -> int

    val display : t -> unit

    (** [add v1 v2] is the element wise addition of v1 and v2
        Raises Invalid_argument if len v1 <> len v2
        Raises Empty_vector if v1 or v2 is empty*)
    val add : t -> t -> t

    val add_ip : t -> t -> unit

    (** [sub v1 v2] is the element wise subtraction of v1 and v2
        Raises Invalid_argument if len v1 <> len v2
        Raises Empty_vector if v1 or v2 is empty*)
    val sub : t -> t -> t

    val sub_ip : t -> t -> unit

    (** [mul v s] is the the vector v multiplied by the scalar s
        Raises Empty_vector if v is empty*)
    val scl : t -> elt -> t

    val scl_ip : t -> elt -> unit

    val linear_comb : t array -> elt array -> t

    val linear_comb_fma : t array -> elt array -> t

    (** [lerp_e p1 p2 t] is the point on the line between e1 e2 at t distance from e1
         Example: lerp 0. 1. 0.5 is 0.5
                 lerp 0. 1. 0 is 0.5
                 lerp 21. 42. 0.3 is 27.3
    *)
    val lerp_p : elt -> elt -> elt -> elt

    (** [lerp v1 v2 t] is the point on the line *)
    val lerp : t -> t -> elt -> t

    (* * [dot v1 v2] is the dot product of v1 with v2 *)
    val dot : t -> t -> elt

    (* * [dot_fma v1 v2] is the dot product of v1 with v2 *)
    val dot_fma : t -> t -> elt

    (** [of_array arr] is the Vector containing the same elements as arr*)
    val of_array : elt array -> t

    (** [of_list lst] is the Vector containing the same elements as lst*)
    val of_list : elt list -> t
end

(* module Make (Ord : OrderedType) : S with type key = Ord.t *)
module Make (Element : Field) : S with type elt = Element.t
