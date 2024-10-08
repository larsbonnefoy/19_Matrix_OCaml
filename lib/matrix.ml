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
    val switch_row: int -> int -> t -> unit
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
    val mul_mat : t -> t -> t
    val mul_mat_ip : t -> t -> unit
    val lup_decompo : t -> t * int array
    val lup_decompo_ip : t -> int array
    val lup_solve: t -> int array -> v -> v
    val trace : t -> elt
    val transpose : t -> t
    val transpose_ip : t -> unit
    val row_echelon_form: t -> t
    val row_echelon_form_ip: t -> unit
    val determinant: t -> elt
    val inverse: t -> t
    val inverse_ip: t -> unit
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

    (** [size m] is a tuple (row, col) representing the number of rows and cols of the matrix m*)
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

    let row_col_check i start finish = function 
        | Matrix {size=_; _} when start > finish-> raise (Invalid_argument "Start after finish");
        | Matrix {size=(_, col); _} when start < 0 || start > col - 1 -> raise (Invalid_argument "Start out of bounds");
        | Matrix {size=(_, col); _} when finish < 0 || finish > col - 1 -> raise (Invalid_argument "Finish out of bounds");
        | Matrix {size=(row, _); _} when i < 0 || i > row - 1 -> raise (Invalid_argument "Row out of bounds");
        | Matrix {size = _; repr=_} as m -> m

    (** [mapi_row f i start finish m] applies f inplace to row [i] to index first and then to the element. Index is taken between [[start; finish]] *)
    let mapi_row f i ~start:s ~finish:fn ~matrix:m = 
        match (row_col_check i s fn m) with
        | Matrix {size = _; repr} -> begin
            let v = repr.(i) in
            for j = s to fn do 
                Vector.get v j |> fun elt -> (f j) elt |> Vector.set v j
            done
        end

        
    (** [iteri_col f i start finish m] iterates over column [i] of matrix [m] by applying [f] to index of row first and then to element at index between [[start; finish]]. !! Validity of start and finish not checked, nor is column index*)
    let iteri_col f i ~start:st ~finish:fn = function 
        | Matrix { size = (_, _) ; repr} -> begin
            for y = st to fn do                                 (*loop over rows*)
                Vector.get repr.(y) i |> f y
            done;
        end
            
    (** [iteri_row f i start finish m] iterates over row [i] of matrix [m] by applying [f] to index of row first and then to element at index between [[start; finish]]. !! Validity of start and finish not checked, nor is row index*)
    let iteri_row f i ~start:st ~finish:fn = function 
        | Matrix { size = (_, _) ; repr} -> begin
            let row = repr.(i) in
            for y = st to fn do                                 (*loop over cols*)
                Vector.get row y |> f y                         (*could be made clearner with iter of vec directly but needs to be impl in vec class*)
            done;
        end

    (** [fold_diag f acc m] is the fold left on m applying f to its diagonal elements *)
    let fold_diag f acc = function 
        | Matrix { size = (r, c) ; repr} -> begin
            let acc_ref = ref acc in
            for i = 0 to (Int.min r c) - 1 do 
                let elt = Vector.get repr.(i) i in 
                acc_ref := f !acc_ref elt
            done;
            !acc_ref
        end

    (* [make r c v] is a matrix with r rows and c columns with filled with value v*)
    let make r c v = 
        let array2d = Array.make r (Vector.make c Element.zero) in
        for i = 0 to r - 1 do 
            array2d.(i) <- Vector.make c v
        done;
        Matrix({size=(r, c); repr = array2d}) 

    (** [init r c f] initalises a new matrix where element a at row r and column c if the result of f r c *)
    let init (r : int) (c : int) (f : int -> int -> elt) = 
        return (r, c) (Array.init r (fun r -> Vector.init c (f r)))

    let check_row_col m r c = 
        match m with
        | Matrix {size = (row, _); _} when r < 0 || r > row - 1 -> raise (Invalid_argument "row out of bounds" )
        | Matrix {size = (_, col); _} when c < 0 || c > col - 1 -> raise (Invalid_argument "col out of bounds" )
        | Matrix {size = _; repr = _} as m -> m

    (** [set m r c x] sets row r and column c of matrix m to x*)
    let set m r c x =
        match (check_row_col m r c) with
        | Matrix {size = _; repr} -> begin
            let v = repr.(r) in
            Vector.set v c x
        end

    (**[set_col m c v] sets column c of matrix m to vector v*)
    let set_col m c v = 
        match m with 
        | Matrix {size = (r, _); _} when r <> Vector.length v -> raise (Invalid_argument (Printf.sprintf "Vector length %d does not match number of rows %d" (Vector.length v) r))
        | Matrix {size = (r, _); _} -> begin 
            for i = 0 to r - 1 do 
                Vector.get v i |> set m i c 
            done;
        end

    (** [get m r c] returns element at row r and c from matrix m*)
    let get m r c = 
        match (check_row_col m r c) with
        | Matrix {size = _; repr} -> begin
            let v = repr.(r) in
            Vector.get v c
        end

    let copy = function 
        | Matrix {size = (r, c); _ } as m -> init r c (fun row col -> get m row col)

    let is_empty = function
        | Matrix {size = (0, _); _ } | Matrix { size = (_, 0); _} -> true
        | Matrix {size = (_, _); _ }  -> false

    let is_square = function
        | Matrix {size = (r, c); _ } when r = c -> true 
        | _ -> false

    let id r c = init r c (fun r c -> if r = c then Element.one else Element.zero)


    (**[switch_row_range r1 r2 start finish m] values in rows r1 and r2 in m between [[start; finish]]*)
    let switch_row_range r1 r2 s f = function 
        | Matrix {size = (row, _); _ } when r1 < 0 || r1 > row - 1 -> raise (Invalid_argument "switch_row: r1 out of bounds")
        | Matrix {size = (row, _); _ } when r2 < 0 || r2 > row - 1 -> raise (Invalid_argument "switch_row: r2 out of bounds")
        | Matrix {size = _; repr} as m -> begin
            let mapi_range_appl = mapi_row ~start:s ~finish:f ~matrix:m in
            let tmp_r1 = Vector.copy repr.(r1) in
            let copy_r2 = (fun i _ -> Vector.get repr.(r2) i) in
            mapi_range_appl (copy_r2) r1;
            let copy_r1 = (fun i _ -> Vector.get tmp_r1 i) in
            mapi_range_appl (copy_r1) r2;
        end

    let switch_row r1 r2 = function
        | Matrix {size = (_, col); _ } as m -> switch_row_range r1 r2 0 (col - 1) m

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

    (* Unreadable impl but works *)
    let mul_mat m1 m2 = 
        match (size m1, size m2) with 
        | ( (r1, c1), (r2, c2) ) when c1 <> r2 ->
                raise (Invalid_argument (Printf.sprintf "mul_mat: invalid dimensions %dx%d cannot be multipled with %dx%d" r1 c1 r2 c2));
        | ( (r1, c1), (_, c2)) -> begin 
            let new_m = make r1 c2 Element.zero in 
            for i = 0 to r1 - 1 do                  (*loop over each row of m1*)
                for j = 0 to c2 - 1 do              (*loop over each col of m2*)
                    let row_col_mul index elt_m1 =  (*index of current elt being mult*)
                        let acc = get new_m i j in  (*current value at position i j in new_m, used as accumulator*)
                        let elt_m2 = get m2 index j in
                        Element.fma elt_m1 elt_m2 acc
                        |> set new_m i j   
                    in
                    iteri_row row_col_mul i ~start:0 ~finish:(c1 - 1) m1;
                done;
            done;
            new_m
        end

    (* m1 and m2 must be square matrices of the same size *)
    let mul_mat_ip m1 m2 = 
        if not ((is_square m1) && (is_square m2)) then raise (Invalid_argument "mul_mat_ip: matrix has to be square")
        else match (mul_mat m1 m2) with
        | Matrix {size=(r, _); repr=tmp_repr} -> begin
            match m1 with 
            | Matrix {size=_; repr=m1_repr} -> begin
                for i = 0 to r - 1 do 
                m1_repr.(i) <- tmp_repr.(i)
                done;
            end
        end

    (** [lup_decompo_ip m] is the in place LUP decomposition of m. LU is written to m and expression evaluates to P the pivot matrix. P is an int array of length (row m) + 1 where value at entry gives position of "1" in permutation matrix *)
    let lup_decompo_ip = function 
        | Matrix {size=(r, c); _} as m -> begin 
            (*Permutation array of size r + 1 where value y at index i means that the ith row has a 1 at column y. Last value is used to store the number of permutations*)
            let p = Array.init (r + 1) (fun x -> if x <> r then x else 0) in 
            (*loop over all rows*)
            for k = 0 to r - 1 do
                (*Find where the max element in column K*)
                let max = ref Element.zero in
                let max_row = ref 0 in 
                let find_max i elt = 
                    let elt_abs = Element.abs elt in 
                    if (elt_abs) > !max then (max := elt_abs; max_row := i) 
                in
                iteri_col find_max k ~start:k ~finish:(r - 1) m;        (*We always start at diagonal element, this is why we want column k, and start at element k*)
                if !max = Element.zero then raise (Failure "lup decompo: Singular matrix");
                (*Switch current row with max element row*)
                if k <> !max_row then
                    let tmp = p.(k) in 
                    p.(k) <- p.(!max_row); p.(!max_row) <- tmp;
                    switch_row k !max_row m;
                    p.(r) <- p.(r) + 1;
                else ignore (); (*required to match the above if clause*)
                (* Need to update Schurs compl, we loop over each row under the current row k *)
                let pivot = get m k k in
                for i = k + 1 to r - 1 do  
                    (* Column under pivot is divided by pivot value*)
                    get m i k 
                    |> (fun e -> Element.div e pivot) 
                    |> set m i k;
                    (* Schur compl for n - 1 matrix *)
                    let schur_f j elt = 
                        let top = get m i k in 
                        let left = get m k j in 
                        Element.fma (Element.neg top) left elt;
                    in
                    mapi_row schur_f i ~start:(k + 1) ~finish:(c - 1) ~matrix:m
                done;
            done;
            p
        end

    (**[lup_decompo m] returns a tuple (LU, P) where LU is the LU decomposition and P the permutation matrix. 
       Permutation matrix is of length l = (row m) where values up to [l - 1] are the position of 1s in the permutation matrix 
       and the l th value is the number of permutation*)
    let lup_decompo m = 
        let cpy_m = copy m in 
        let p = lup_decompo_ip cpy_m in 
        (cpy_m, p)

    (** Returns the trace of matrix m which is the sum of its diagonal elements*)
    let trace = fold_diag (fun acc elt -> Element.add acc elt) Element.zero

    let transpose = function 
        | Matrix {size=(r, c); _} as m -> begin 
            let new_m = make c r Element.zero in
            for col = 0 to c - 1 do                               (*loop over every col*)
                let set_elt row elt = set new_m col row elt in        (* ith col becomes ith row *)
                iteri_col set_elt col ~start:0 ~finish:(r - 1) m;
            done;
        new_m
        end

    let transpose_ip = function
        | Matrix {size=_; repr = repr_og} as m when is_square m -> begin
            match transpose m with 
            | Matrix {size=(r, _); repr= repr_transposed} -> begin 
                for i = 0 to r - 1 do 
                    repr_og.(i) <- repr_transposed.(i)
                    done
                end
        end 
        | _ -> raise (Invalid_argument "transpose_ip: matrix should be square to transpose in place")

    let row_echelon_form_ip = function
        | Matrix {size=(r, _); repr} as m -> begin
            for i = 0 to r - 1 do 
                (*Need to skip a column when pivot = 0 => curr_col != i*)
                let rec find_pivot col_index = 
                    try 
                        match (get m i col_index) with
                        | _ as pvt when pvt = Element.zero -> find_pivot (col_index + 1)
                        | _ as pvt -> Some (pvt, col_index)
                    with 
                        | Invalid_argument _ -> None (*Case for a singular Matrix, ignore it atm*)
                in
                match find_pivot i with 
                | Some (pivot, curr_col) -> begin 
                    (* Scale row i by inverse of pvt *)
                    let inverse v = Element.div Element.one v in
                    Vector.scl_ip repr.(i) (inverse pivot);
                    (*loop over all rows <> i to remove potential values*)
                    for j = 0 to r - 1 do 
                        if j <> i then begin
                            let lead = get m j curr_col in 
                            Vector.sub_ip repr.(j) (Vector.scl repr.(i) lead)
                        end
                    done;
                end
                | None -> ignore ()
            done
        end

    let row_echelon_form m =
            let copy_m = copy m in 
            row_echelon_form_ip copy_m;
            copy_m

    let determinant m = 
        match (size m) with 
        | (1, 1) -> get m 0 0
        | (2, 2) -> begin 
                let a = Element.mul (get m 0 0) (get m 1 1) in 
                let b = Element.mul (get m 0 1) (get m 1 0) in
                Element.sub a b
            end 
        | (r, c) when r = c -> begin
                try 
                    let (lu, p) = lup_decompo m in 
                    let diag = fold_diag (fun acc elt -> Element.mul acc elt) Element.one lu in 
                    if p.((Array.length p - 1)) mod 2 = 0 then diag else Element.neg diag
                with
                    | Failure _ -> Element.zero
            end
        | (_, _) -> raise (Invalid_argument "determinant: m is not a square matrix")

    (**Solves for x in [LUx = Pb] where LU and P are the matrix and the permutation array returned by lup decompo *)
    let lup_solve lu p b = 
        match lu with 
        | Matrix{size = (r, _); _} -> begin 
            let y = Vector.make r Element.zero in
            (*Forward subsitut to solve Ly = Pb*)
            for i = 0 to r - 1 do 
                let b_perm = Vector.get b p.(i) in 
                let acc_prev = ref Element.zero in  (*Accumulator to compute previously solved equations*)
                for j = 0 to i do 
                    let substit = Element.mul (get lu i j) (Vector.get y j) in
                    acc_prev := Element.add !acc_prev substit
                done;
                Vector.set y i (Element.sub b_perm !acc_prev)
            done;
            (*Backward subsitut to solve Ux = y*)
            let x = Vector.make r Element.zero in 
            for i = r - 1 downto 0 do 
                let acc_prev = ref Element.zero in 
                for j = i + 1 to r - 1 do 
                    let substit = Element.mul (get lu i j) (Vector.get x j) in
                    acc_prev := (Element.add !acc_prev substit)
                done;
                let norm = get lu i i in 
                let y_val = Vector.get y i in 
                let div x y = Element.div y x in  (*flip div to make pipe cleaner*)
                Element.sub y_val !acc_prev |> div norm |> Vector.set x i
            done;
            x
            end

    let inverse_ip = function
        | Matrix{size = (r, c); _} as m when r = c -> begin 
            let (lu, p) = lup_decompo m in 
            for j = 0 to c - 1 do 
                let id = Vector.init r (fun i -> if i = j then Element.one else Element.zero) in
                let solution = lup_solve lu p id in 
                set_col m j solution
            done;
            end
        | _ -> raise (Invalid_argument "inverse: matrix is not square")

    let inverse m = 
        let cpy = copy m in 
        inverse_ip cpy;
        cpy

    let rank m = 
        let not_null r = Vector.fold_left (fun acc elt -> acc || (elt <> Element.zero)) false r in
        let null_rows_cntr = ref 0 in
        match (row_echelon_form m) with 
        | Matrix{size = (rows, _); repr} -> begin 
            Array.iter (fun r -> if (not (not_null r)) then incr null_rows_cntr) repr;
            rows - !null_rows_cntr
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
