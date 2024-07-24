(**Required operations on basic type t*)
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
    type elt 
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
    val scl_ip : t -> elt -> unit
    (* val lerp : t -> t -> elt -> t *)
    val of_array : elt array array -> t
    (* val of_list : elt list list -> t *)
end

module Make(Element : ReqOp) = struct
    (** row major representation to match matrix notation *)

    type elt = Element.t

    type t = Matrix of {size : (int * int); m : elt array array}

    (* Monad Operations*)

    (** [return a] encapsulates a into Vector Type*)
    let return (s : int * int ) (m : elt array array) = 
        Matrix({size = s; m = m})

    (**)
    (* (** [bind v op] applies op to the underlying type of v*) *)
    let bind (v : t) (op : elt array array -> t) = 
        match v with
        | Matrix {size = _ ; m = a} -> op a

    (**)
    let ( >>= ) = bind
    (* -------------------------- *)

    let ( * ) = Element.mul
    let ( - ) = Element.sub
    let ( + ) = Element.add

    (** Defined only in Float module, required for other implementation
        fma x y z returns x * y + z *)
    (* let fma = Element.fma *)


    (** [init r c v] is the matrix with r rows and c columns with value v 
        @raises Invalid_argument if r < 0 or c < 0 *)
    let init r c v = Matrix({size=(r, c); m = Array.make_matrix r c v})

    let size = function
        | Matrix {size; _} -> size

    let is_empty = function
        | Matrix {size = (0, _); _ } | Matrix { size = (_, 0); _} -> true
        | Matrix {size = (_, _); _ }  -> false

    let map f = function
        | Matrix {size = s ; m} -> begin 
            let new_m = Array.map (fun sub_a -> Array.map (fun e -> f e) sub_a) m in  
            return s new_m
        end 

    let map_ip f = function
        | Matrix {size = _; m} -> begin 
            Array.iter (fun sub_a -> Array.map_inplace (fun e -> f e) sub_a) m
        end


    let map2 f m1 m2 = 
        m1 >>= fun array_2d_1 -> 
        m2 >>= fun array_2d_2 -> 
        let elt_fun = fun e1 e2 -> f e1 e2 in
        let map_rows = fun a1 a2 -> Array.map2 elt_fun a1 a2 in
        return (size m1) (Array.map2 map_rows array_2d_1 array_2d_2)

    let to_string = function
        | Matrix {size = (0, _); _ } | Matrix { size = (_, 0); _} -> "| ... |\n"
        | Matrix { size = _; m} -> begin
            let row_str a = 
                let format_elt = fun acc e -> acc ^ (Printf.sprintf "%s " (Element.to_string e)) in
                (Array.fold_left format_elt "| " a) ^ "|\n"
            in
            Array.fold_left (fun acc e -> acc ^ row_str e) "" m
        end

    let display m = Printf.printf "%s" (to_string m)
    
    let add m1 m2 = map2 ( + ) m1 m2

    let sub m1 m2 = map2 ( - ) m1 m2

    let scl m s = map ( ( * ) s) m

    let scl_ip m s = map_ip ( ( * ) s) m

    (* * Makes a column of *)
    let of_array (a : elt array array) = 
        let rows = Array.length a in 
        let base_col = Array.length a.(0) in 
            let check_f sub_a = 
                if (Array.length sub_a) <> base_col 
                then raise (Invalid_argument "of_array: rows do not match") in
            Array.iter check_f a;
        return (rows, base_col) a

end
