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
    val abs : t -> t
    val sqrt : t -> float
    val to_string : t -> string
    val to_float : t -> float
end

module type S = sig
    (** elt is the type contained in Vector*)
    type elt 

    (** Is of type Vector 'a or Empty*)
    type t

    (** [make s v] creates a new vector of size s filled with value v
        @raise Invalid_argument if s < 0 or s > Sys.max_array_length *)
    val make : int -> elt -> t

    (**[init n f] creates a new vector of length n where elements are initalised to f i where i is the index from 0 to n - 1*)
    val init : int -> (int -> elt) -> t

    (** [is_empty v] is true if v is Empty, false if v is Vector _*)
    val is_empty : t -> bool

    (** [length v] the number of elements contained in v*)
    val length : t -> int

    val display : t -> unit

    val equal: t -> t -> bool

    (**[get v pos] is element at index i in v
    @raises Invalid_argument if n is outside the range 0 to (length a - 1). *)
    val get: t -> int -> elt

    val set : t -> int -> elt -> unit

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

    (**[linear_comb a c] is lc, the linear combination of vectors from array a and scalars of c so that c1 * a1 + c2 * a2 = lc *)
    val linear_comb : t array -> elt array -> t

    (**[linear_comb a c] is lc, the linear combination of vectors from array a and scalars of c so that [c1 * a1 + c2 * a2 = lc]. 
       Uses fuse-add-multiply impl provided in Functor init*)
    val linear_comb_fma : t array -> elt array -> t

    (** [lerp_e p1 p2 t] is the point on the line between e1 e2 at t distance from e1
         Example: lerp 0. 1. 0.5 is 0.5
                 lerp 0. 1. 0 is 0.5
                 lerp 21. 42. 0.3 is 27.3
    *)
    val lerp_p : elt -> elt -> elt -> elt

    (** [lerp v1 v2 t] is the point on the line *)
    val lerp : t -> t -> elt -> t

    val lerp_ip : t -> t -> elt -> unit

    (* * [dot v1 v2] is the dot product of v1 with v2 *)
    val dot : t -> t -> elt

    (* * [dot_fma v1 v2] is the dot product of v1 with v2 *)
    val dot_fma : t -> t -> elt
    
    val norm_1 : t -> elt

    val norm : t -> float

    val norm_inf : t -> elt

    val cos : t -> t -> float

    val cross_product : t -> t -> t

    val cross_product_ip : t -> t -> unit

    val to_string : t -> string

    (** [of_array arr] is the Vector containing the same elements as arr*)
    val of_array : elt array -> t

    (** [of_list lst] is the Vector containing the same elements as lst*)
    val of_list : elt list -> t

    val copy : t -> t

    val map : (elt -> elt) -> t -> t

    val map_ip : (elt -> elt) -> t -> unit

    val map2: (elt -> elt -> elt) -> t -> t -> t

    val map2_ip : (elt -> elt -> elt) -> t -> t -> unit

    val fold_left: ('a -> elt -> 'a) -> 'a -> t -> 'a
end

(* module Make (Ord : OrderedType) : S with type key = Ord.t *)
module Make (Element : Field) : S with type elt = Element.t
