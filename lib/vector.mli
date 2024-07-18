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

    (** Is of type Vector 'a or Empty*)
    type 'a t

    val init : int -> elt -> 'a t

    val empty : 'a t

    (** [is_empty v] is true if v is Empty, false if v is Vector _*)
    val is_empty : 'a t -> bool

    (** [length v] the number of elements contained in v*)
    val length : 'a t -> int

    val display : 'a t -> unit

    (** [add v1 v2] is the element wise addition of v1 and v2
        Raises Invalid_argument if len v1 <> len v2
        Raises Empty_vector if v1 or v2 is empty*)
    val add : 'a t -> 'a t -> 'a t

    (** [sub v1 v2] is the element wise subtraction of v1 and v2
        Raises Invalid_argument if len v1 <> len v2
        Raises Empty_vector if v1 or v2 is empty*)
    val sub : 'a t -> 'a t -> 'a t

    (** [mul v s] is the the vector v multiplied by the scalar s
        Raises Empty_vector if v is empty*)
    val scl : 'a t -> elt -> 'a t

    val linear_comb : 'a t array -> elt array -> 'b t

    (** [of_array arr] is the Vector containing the same elements as arr*)
    val of_array : elt array -> 'a t

    (** [of_list lst] is the Vector containing the same elements as lst*)
    val of_list : elt list -> 'a t
end

(* module Make (Ord : OrderedType) : S with type key = Ord.t *)
module Make (Element : Field) : S with type elt = Element.t
