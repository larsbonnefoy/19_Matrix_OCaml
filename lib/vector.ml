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
    val fma : t -> t -> t -> t
    val abs : t -> t
    val sqrt : t -> float
    val to_string : t -> string
    val to_float : t -> float
end

module type S = sig
    type elt 
    type t
    val make : int -> elt -> t
    val init : int -> (int -> elt) -> t
    val is_empty : t -> bool
    val length : t -> int
    val display : t -> unit
    val equal: t -> t -> bool
    val get: t -> int -> elt
    val set : t -> int -> elt -> unit
    val add : t -> t -> t
    val add_ip : t -> t -> unit
    val sub : t -> t -> t
    val sub_ip : t -> t -> unit
    val scl : t -> elt -> t
    val scl_ip : t -> elt -> unit
    val linear_comb : t array -> elt array -> t
    val linear_comb_fma : t array -> elt array -> t
    val lerp_p : elt -> elt -> elt -> elt
    val lerp : t -> t -> elt -> t
    val lerp_ip : t -> t -> elt -> unit
    val dot : t -> t -> elt
    val dot_fma : t -> t -> elt
    val norm_1 : t -> elt
    val norm : t -> float
    val norm_inf : t -> elt
    val cos : t -> t -> float
    val cross_product : t -> t -> t
    val to_string : t -> string
    val of_array : elt array -> t
    val of_list : elt list -> t
    val copy : t -> t
    val map : (elt -> elt) -> t -> t
    val map_ip : (elt -> elt) -> t -> unit
    val map2: (elt -> elt -> elt) -> t -> t -> t
    val map2_ip : (elt -> elt -> elt) -> t -> t -> unit
end

module Make(Element : Field) = struct

    type elt = Element.t

    type t = Vector of elt array

    (* Monad Operations*)

    (** [return a] encapsulates a into Vector Type*)
    let return (a : elt array) = Vector a

    (** [bind v op] applies op to the underlying type of v*)
    let bind (v : t) (op : elt array -> t) = 
        match v with
        | Vector a -> op a

    let ( >>= ) = bind
    (* -------------------------- *)

    let ( * ) = Element.mul
    let ( - ) = Element.sub
    let ( + ) = Element.add

    (** Defined only in Float module, required for other implementation
        fma x y z returns x * y + z *)
    let fma = Element.fma

    (* Redfine standard Array operations as operations on Vector*)
    let length = function
        | Vector s -> (Array.length s)

    (**[get v pos] is element at index i in v
    @raises Invalid_argument if n is outside the range 0 to (length a - 1). *)
    let get v i = 
        match v with
        | Vector s -> Array.get s i

    (**[set v i x] modifies v in place and replaces element at index i with x *)
    let set v i x = 
        match v with 
        | Vector s -> Array.set s i x

    let equal v1 v2 = 
        match (v1, v2) with 
        | (Vector a1, Vector a2) -> Array.for_all2 (fun a b -> a = b) a1 a2

    let map f v = 
        v >>= fun a -> return (Array.map f a)

    let map_ip f = function
        | Vector a -> Array.map_inplace f a

    (** [map2 f v1 v2] is the new vector v where f has been applied element wise to v1 and v2*)
    let map2 f v1 v2 = 
        v1 >>= fun a1 -> 
        v2 >>= fun a2 ->
        return (Array.map2 f a1 a2)

    (** [map2_ip f v1 v2] applies f element wise to to v1 and v2 and stores results into v1*)
    let map2_ip f v1 v2 = 
        (* Need to redefine - operator to make it work with ints *)
        let ( - ) = Int.sub in  
        match (v1, v2) with
        | (v1, v2) when (length v1) <> (length v2) -> raise (Invalid_argument "map2_ip: v1 and v2 are different lengths")
        | (Vector a1, Vector a2) -> begin
            for i = 0 to (Array.length a1) - 1 do 
                a1.(i) <- (f a1.(i) a2.(i))
            done
        end

    (** Applies f elementwise to v1 and v2 and stores result in acc 
        Mutates acc in place
        acc := f v1 v2 acc*)
    let map2_acc f v1 v2 acc = 
        let l1 = length v1 in 
        let l2 = length v2 in 
        if l1 <> l2 then
            invalid_arg "map2_acc: vectors must have the same length"
        else begin
            let ( - ) = Int.sub in
            for x = 0 to l1 - 1 do 
                acc := f (get v1 x) (get v2 x) !acc
            done
        end
        
    let fold_left f init v = 
        match v with 
        | Vector a -> Array.fold_left f init a

    (* -------------------------- *)

    (** [make s v] creates a new vector of size s filled with value v
        @raise Invalid_argument if s < 0 or s > Sys.max_array_length *)
    let make (s : int) (v : elt) = return (Array.make s v)

    (**[init n f] creates a new vector of length n where elements are initalised to f i where i is the index from 0 to n - 1*)
    let init n f = return (Array.init n f)

    let is_empty v = if length v = 0 then true else false

    let to_string = function
        | Vector v -> ( Array.fold_left (fun acc elem -> acc ^ (Element.to_string elem) ^ " ") "" v )

    let display v = Printf.printf "[ %s]\n" (to_string v)

    let add v1 v2 = map2 ( + ) v1 v2

    let add_ip v1 v2 = map2_ip ( + ) v1 v2

    let sub v1 v2 = map2 ( - ) v1 v2

    let sub_ip v1 v2 = map2_ip ( - ) v1 v2

    let scl v s = map ( ( * ) s ) v

    let scl_ip v s = map_ip ( ( * ) s) v

    let linear_comb (v : t array) (c : elt array) = 
        let neutral_vector = make (length v.(0)) Element.zero in 
        Array.map2 scl v c |> Array.fold_left add neutral_vector

    (** Will raise Index_out_of_bound if size does not match*)
    let linear_comb_fma (a : t array) (c : elt array) = 
        let a_size = Array.length a in
        let c_size = Array.length c in
        if  a_size = 0 then raise (Failure "linear_comb_fma: Empty input array");
        if  a_size <> c_size then raise (Failure "linear_comb_fma: a and c are of different size");
        let ( - ) = Int.sub in  
        let v_size = length a.(0) in
        let acc = Array.make (v_size) Element.zero in
        for x = 0 to v_size - 1 do                      (* Loops over each position in vector*)
            for y = 0 to a_size - 1 do                  (*Loops over scalar array*)
                let scalar = c.(y) in
                let vector_entry = get a.(y) x in       (* xth position in yth vector *)
                acc.(x) <- fma vector_entry scalar acc.(x) 
            done
        done;
        return acc

    let lerp_p (p1 : elt) (p2 : elt) (t : elt) = fma (p2 - p1) t p1

    let lerp v1 v2 t = map2 (fun e1 e2 -> lerp_p e1 e2 t) v1 v2

    let lerp_ip v1 v2 t = map2_ip (fun e1 e2 -> lerp_p e1 e2 t) v1 v2

    let dot v1 v2 = map2 ( * ) v1 v2 |> fold_left ( + ) Element.zero

    let dot_fma v1 v2 = 
        let acc = ref Element.zero in 
        map2_acc fma v1 v2 acc;
        !acc

    (** [norm_1 v] is the Manhattan norm of v*)
    let norm_1 v = fold_left ( fun acc x -> acc + Element.abs x) Element.zero v

    (** [norm v] is the Euclidan norm of v*)
    let norm v = (dot_fma v v) |> Element.sqrt

    (** [norm_inf v] is the maximum norm of v*)
    let norm_inf v = 
            let max_abs acc x = if (Element.abs x) > acc then Element.abs x else acc in
            fold_left max_abs (get v 0) v

    let cos v1 v2 = 
        match (v1, v2) with
        | (Vector _, Vector _) -> begin
                let num = dot_fma v1 v2 in 
                let denom = (norm v1) *. (norm v2) in 
                (Element.to_float num) /. denom
            end

     (*           | i  j  k  | *)
     (*   a x b = | a1 a2 a3 | *)
     (*           | b1 b2 b3 | *)
     (*  *)
     (*   a x b = (a2b3 - a3b2)i - (a1b3 - a3b1)j + (a1b2 - a2b1)k *)
     (*    *)
     (*   | s1 |   | a2b3 - a3b2 | *)
     (*   | s2 | = | a3b1 - a1b3 | *)
     (*   | s3 |   | a1b2 - a2b1 | *)
    let cross_product v1 v2 = 
            let x = Element.mul (get v1 1) (get v2 2) - Element.mul (get v1 2) (get v2 1) in 
            let y = Element.mul (get v1 2) (get v2 0) - Element.mul (get v1 0) (get v2 2) in 
            let z = Element.mul (get v1 0) (get v2 1) - Element.mul (get v1 1) (get v2 0) in 
        return [|x; y; z|]


    (** [of_array arr] is the Vector containing the same elements as arr*)
    let of_array (arr : elt array) = 
        if Array.length arr = 0 then raise (Invalid_argument "of_array: Empty array")
        else return arr

    (** [of_list lst] is the Vector containing the same elements as lst*)
    let of_list (lst : elt list) = 
        if List.length lst = 0 then raise (Invalid_argument "of_list: Empty list")
        else return (Array.of_list lst)

    let copy = function
        | Vector v -> return (Array.copy v)
        

end
