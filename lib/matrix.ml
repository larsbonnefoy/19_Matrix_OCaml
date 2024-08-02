module type S = sig
    type elt 
    type t
    type v
    val make : int -> int -> elt -> t
    val size : t -> int * int
    val is_empty : t -> bool
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
    val of_vector_array : v array -> t
    val of_array : elt array array -> t
end

module Make(Vector : Vector.S) = struct
    (* Row major representation, each row is represented by a vector *)
    type v = Vector.t
    type t = Matrix of {size: (int * int); repr: v array}
    type elt = Vector.elt

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

    (* [make r c v] is a matrix with r rows and c columns with filled with value v*)
    let make r c v = Matrix({size=(r, c); repr = Array.make r (Vector.make c v)})

    let is_empty = function
        | Matrix {size = (0, _); _ } | Matrix { size = (_, 0); _} -> true
        | Matrix {size = (_, _); _ }  -> false

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
