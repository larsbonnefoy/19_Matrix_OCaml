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
    val init : int -> elt -> t
    val empty : t
    val is_empty : t -> bool
    val length : t -> int
    val display : t -> unit
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
    val of_array : elt array -> t
    val of_list : elt list -> t
end

module Make(Element : Field) = struct

    type elt = Element.t

    type t = Empty | Vector of elt array

    (* Monad Operations*)

    (** [return a] encapsulates a into Vector Type*)
    let return (a : elt array) = 
        if Array.length a = 0 then Empty
        else Vector a

    (** [bind v op] applies op to the underlying type of v*)
    let bind (v : t) (op : elt array -> t) = 
        match v with
        | Empty -> Empty
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
        | Empty -> 0
        | Vector s -> (Array.length s)

    (**[get v pos] is element at index i in v*)
    let get v i = 
        match v with
        | Empty -> raise (Failure "Empty Vector")
        | Vector s -> Array.get s i

    let map f v = 
        v >>= fun a -> return (Array.map f a)

    let map_ip f = function
        | Empty -> ()
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
        | (Empty, _) | (_, Empty)  -> raise (Invalid_argument "map2_ip: v1 or v2 is empty")
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
        | Empty -> raise (Failure "Empty Vector")
        | Vector a -> Array.fold_left f init a

    (* -------------------------- *)

    (** [init s v] creates a new vector of size s filled with value v
        @raise Invalid_argument if s < 0 or s > Sys.max_array_length *)
    let init (s : int) (v : elt) = return (Array.make s v)

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

    let add v1 v2 = map2 ( + ) v1 v2

    let add_ip v1 v2 = map2_ip ( + ) v1 v2

    let sub v1 v2 = map2 ( - ) v1 v2

    let sub_ip v1 v2 = map2_ip ( - ) v1 v2

    let scl v s = map ( ( * ) s ) v

    let scl_ip v s = map_ip ( ( * ) s) v

    let linear_comb (v : t array) (c : elt array) = 
        let neutral_vector = init (length v.(0)) Element.zero in 
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
        | (Empty, _ ) | (_, Empty) -> raise (Failure "cos: v1 or v2 is empty")
        | (Vector _, Vector _) -> begin
                let num = dot_fma v1 v2 in 
                let denom = (norm v1) *. (norm v2) in 
                (Element.to_float num) /. denom
            end


    (** [of_array arr] is the Vector containing the same elements as arr*)
    let of_array (arr : elt array) = return arr

    (** [of_list lst] is the Vector containing the same elements as lst*)
    let of_list (lst : elt list) = 
        if List.length lst = 0 then empty 
        else Vector(Array.of_list lst)

end
