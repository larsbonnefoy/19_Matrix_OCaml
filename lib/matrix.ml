module type S = sig
    type elt 
    type t
    type v
    val make : int -> int -> elt -> t
    val init : int -> int -> (int -> int -> elt) -> t
    val size : t -> int * int
    val is_empty : t -> bool
    val is_square : t -> bool
    val id : int -> int -> t
    val to_string : t -> string
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
    val lu_decompo : t -> t * t
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
end

module Make(Vector : Vector.S) (Element : EltOp with type t = Vector.elt) = struct
    (* Row major representation, each row is represented by a vector *)
    type v = Vector.t
    type t = Matrix of {size: (int * int); repr: v array}
    type elt = Element.t

    let return (s : int * int ) (m : v array) = 
        Matrix({size = s; repr = m})

    (* let bind (v : t) (op : elt array array -> t) =  *)
    let bind (m : t) (op : v array -> t) = 
        match m with
        | Matrix{size = _; repr} -> op repr

    let ( >>= ) = bind

    let size = function
        | Matrix {size; _} -> size

    let map f m = 
        let s = size m in
        m >>= fun a -> return s (Array.map f a)

    let map_ip (f : v -> unit) = function
    | Matrix {size = _; repr} -> Array.iter f repr

    let map2 f m1 m2 =
        let s = size m1 in
        m1 >>= fun a1 ->
        m2 >>= fun a2 -> 
        return s (Array.map2 f a1 a2)

    let map2_ip f m1 m2 = 
        if size m1 <> size m2 then raise (Invalid_argument "map2_ip: m1 and m2 are of different size");
        match (m1, m2) with
        | (Matrix {size = s1 ; repr = a1} , Matrix {size = _ ; repr = a2}) -> begin
            for i = 0  to (fst s1) - 1 do 
                a1.(i) <- f a1.(i) a2.(i)
            done
        end

    (** [map_partial_row f i start finish m] applies f inplace to row i of matrix m between cols [start; end] *)
    (* let map_partial_row f i start finish = function  *)
    (*     | Matrix {size=_; _} when start > finish-> raise (Invalid_argument "map_partial_row: start after finish"); *)
    (*     | Matrix {size=(_, col); _} when start < 0 || start > col - 1 -> raise (Invalid_argument "map_partial_row: start out of bounds"); *)
    (*     | Matrix {size=(_, col); _} when finish < 0 || finish > col - 1 -> raise (Invalid_argument "map_partial_row: finish out of bounds"); *)
    (*     | Matrix {size=(row, _); _} when i < 0 || i > row - 1 -> raise (Invalid_argument "map_partial_row: row out of bounds"); *)
    (*     | Matrix {size = _; repr} -> begin *)
    (*         let v = repr.(i) in *)
    (*         for j = start to finish do  *)
    (*             Vector.get v j |> fun elt -> f elt |> Vector.set v j *)
    (*         done *)
    (*     end *)

    (** [map_partial_col f i start finish m] applies f inplace to col i of matrix m between rows [start; end] .*)
    (* let map_partial_col f i start finish = function  *)
    (*     | Matrix {size=_; _} when start > finish-> raise (Invalid_argument "map_partial_col: start after finish"); *)
    (*     | Matrix {size=(row, _); _} when start < 0 || start > row - 1 -> raise (Invalid_argument "map_partial_col: start out of bounds"); *)
    (*     | Matrix {size=(row, _); _} when finish < 0 || finish > row - 1 -> raise (Invalid_argument "map_partial_col: finish out of bounds"); *)
    (*     | Matrix {size=(_, col); _} when i < 0 || i > col - 1 -> raise (Invalid_argument "map_partial_col: col out of bounds"); *)
    (*     | Matrix {size = _; repr} -> begin *)
    (*         for j = start to finish do  *)
    (*             let v = repr.(j) in *)
    (*             Vector.get v i |> fun elt -> f elt |> Vector.set v i *)
    (*         done *)
    (*     end *)
        

    (* [make r c v] is a matrix with r rows and c columns with filled with value v*)
    let make r c v = Matrix({size=(r, c); repr = Array.make r (Vector.make c v)})

    (** [init r c f] initalises a new matrix where element a at row r and column c if the result of f r c *)
    let init (r : int) (c : int) (f : int -> int -> elt) = 
        return (r, c) (Array.init r (fun r -> Vector.init c (f r)))

    let is_empty = function
        | Matrix {size = (0, _); _ } | Matrix { size = (_, 0); _} -> true
        | Matrix {size = (_, _); _ }  -> false

    let is_square = function
        | Matrix {size = (r, c); _ } when r = c -> true 
        | _ -> false

    let id r c = init r c (fun r c -> if r = c then Element.one else Element.zero)

    let set m r c x =
        match m with
        | Matrix {size = (row, _); _} when r < 0 || r > row - 1 -> raise (Invalid_argument "set: row out of bounds" )
        | Matrix {size = (_, col); _} when c < 0 || c > col - 1 -> raise (Invalid_argument "set: col out of bounds" )
        | Matrix {size = _; repr} -> begin
            let v = repr.(r) in
            Vector.set v c x
        end

    (** [get m r c] returns element at row r and c from matrix m*)
    let get m r c = 
        match m with
        | Matrix {size = (row, _); _} when r < 0 || r > row - 1 -> raise (Invalid_argument "set: row out of bounds" )
        | Matrix {size = (_, col); _} when c < 0 || c > col - 1 -> raise (Invalid_argument "set: col out of bounds" )
        | Matrix {size = _; repr} -> begin
            let v = repr.(r) in
            Vector.get v c
        end

    let to_string = function
        | Matrix {size = _; repr} -> begin 
            let row_format v = Printf.sprintf "| %s|\n" (Vector.to_string v) in
            Array.fold_left (fun acc v -> acc ^ row_format v) "" repr
        end 

    let display m = Printf.printf "%s" (to_string m)

    let scl m s = map (fun v -> Vector.scl v s) m

    let scl_ip m s = map_ip (fun v -> Vector.scl_ip v s) m

    let add m1 m2 = map2 (fun v1 v2 -> Vector.add v1 v2) m1 m2

    let add_ip m1 m2 = map2_ip (fun v1 v2 -> Vector.add v1 v2) m1 m2

    let sub m1 m2 = map2 (fun v1 v2 -> Vector.sub v1 v2) m1 m2

    let sub_ip m1 m2 = map2_ip (fun v1 v2 -> Vector.sub v1 v2) m1 m2

    let lerp m1 m2 t = map2 (fun v1 v2 -> Vector.lerp v1 v2 t) m1 m2

    let lerp_ip m1 m2 t = map2_ip (fun v1 v2 -> Vector.lerp v1 v2 t) m1 m2

    let mul_vec (m : t) (v : v) = 
        match m with
        | Matrix {size=(nb_rows, nb_cols); repr=_} when nb_cols <> Vector.length v -> begin
                raise (Invalid_argument (Printf.sprintf "mul_vec: dim of m (%d, %d) do not match size of v (%d)" nb_rows nb_cols (Vector.length v)));
            end 
        | Matrix {size=(nb_rows, _); repr} -> begin
                Vector.init nb_rows (fun i -> Vector.dot_fma repr.(i) v)
            end

    let mul_vec_ip (m : t) (v : v) = 
        match m with
        | Matrix {size=(nb_rows, nb_cols); repr=_} when nb_cols <> Vector.length v -> begin
                raise (Invalid_argument (Printf.sprintf "mul_vec: dim of m (%d, %d) do not match size of v (%d)" nb_rows nb_cols (Vector.length v)));
        end 
        | Matrix {size=(nb_rows, _); repr} -> begin
            let vec_cpy = Vector.copy v in
            for i = 0 to nb_rows - 1 do
                vec_cpy
                |> Vector.dot_fma repr.(i)
                |> Vector.set v i
            done
        end

    let lu_decompo = function 
        | Matrix {size=(rows, cols); _} as a -> begin
            (* if not (is_square a) then raise (Invalid_argument "lup: m is not square"); *)
            let upper = init rows cols (fun r c -> if r <= c then Element.one else Element.zero) in
            let lower = init rows cols (fun r c -> if r >= c then Element.one else Element.zero) in
            display a;
            for k = 0 to rows - 1 do
                let pivot = get a k k in 
                pivot |> set upper k k;
                for i = k + 1 to rows - 1 do 
                    get a i k |> fun x -> Element.div x pivot |> set lower i k;     (* l_ik = a_ik / a_kk where a_ik is v_i*)
                    get a k i |> set upper k i;                                     (* u_ki = a_ki where a_ki is w_i*)
                done;
                for i = k + 1 to rows - 1 do                                        (* Compute the schur complt*)
                    for j = k + 1 to rows - 1 do 
                        let mult = Element.mul (get lower i k) (get upper k j) in 
                        get a i j |> fun x -> Element.sub x mult |> set a i j; 
                    done
                done;
            done;
            (lower, upper)
        end

    let of_vector_array (a : v array) = 
        let nb_rows = Array.length a in 
        if nb_rows = 0 then raise (Invalid_argument "of_array: row = 0");
        let len_col = Vector.length a.(0) in 
        if len_col = 0 then raise (Invalid_argument "of_array: col = 0");

        let check_f sub_a = 
            if (Vector.length sub_a) <> len_col 
            then raise (Invalid_argument "of_array: rows do not match") in
        Array.iter check_f a;

        return (nb_rows, len_col) a

    let of_array (a : elt array array) = 
        let nb_rows = Array.length a in 
        if nb_rows = 0 then raise (Invalid_argument "of_array: row = 0");
        let len_col = Array.length a.(0) in 
        if len_col = 0 then raise (Invalid_argument "of_array: col = 0");

        let create_row i = 
            let v = Vector.of_array a.(i) in
            if Vector.length v <> len_col then raise (Invalid_argument "of_array: rows do not match")
            else v in

        return (nb_rows, len_col) (Array.init nb_rows create_row)
end
