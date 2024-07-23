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
    val empty : t
    val is_empty : t -> bool
    val size : t -> int * int
    val display : t -> unit
    (* val add : t -> t -> t *)
    (* val add_ip : t -> t -> unit *)
    (* val sub : t -> t -> t *)
    (* val sub_ip : t -> t -> unit *)
    (* val scl : t -> elt -> t *)
    (* val scl_ip : t -> elt -> unit *)
    (* val lerp : t -> t -> elt -> t *)
    (* val of_array : elt array array -> t *)
    (* val of_list : elt list list -> t *)
end

module Make(Element : ReqOp) = struct
    (** col major representation to match matrix notation *)

    type elt = Element.t

    type t = Empty | Matrix of {size : (int * int); m : elt array array}

    (* Monad Operations*)

    (** [return a] encapsulates a into Vector Type*)
    (* let return (a : elt array) =  *)
    (*     if Array.length a = 0 then Empty *)
    (*     else Vector a *)
    (**)
    (* (** [bind v op] applies op to the underlying type of v*) *)
    (* let bind (v : t) (op : elt array -> t) =  *)
    (*     match v with *)
    (*     | Empty -> Empty *)
    (*     | Vector a -> op a *)
    (**)
    (* let ( >>= ) = bind *)
    (* -------------------------- *)

    (* let ( * ) = Element.mul *)
    (* let ( - ) = Element.sub *)
    (* let ( + ) = Element.add *)

    (** Defined only in Float module, required for other implementation
        fma x y z returns x * y + z *)
    (* let fma = Element.fma *)

    let init r c v = 
        if r = 0 || c = 0 then Empty 
        else Matrix({size=(r, c); m = Array.make_matrix c r v})

    let empty = Empty

    let is_empty m = if m = Empty then true else false

    let size = function 
        | Empty -> (0, 0)
        | Matrix {size; _} -> size

    let display = function
        | Empty -> Printf.printf "| |"
        | Matrix { size = _; m} -> begin
            let print_internal a = Array.iter (fun e -> Printf.printf "%s " (Element.to_string e)) a in 
            Array.iter print_internal m
        end
end
