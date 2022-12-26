type rotation = | RotateLeft | RotateRight

type action =
    | Move of int
    | Rotate of rotation

let parse_path path =
    let parse_digits digits =
        Move (digits |> List.rev |> List.to_seq |> String.of_seq |> int_of_string)
    in
    let rec impl digits path = match path with
        | [] -> [parse_digits digits]
        | ('L' as x) :: xs | ('R' as x) :: xs ->
            (parse_digits digits) :: (if x = 'L' then Rotate RotateLeft else Rotate RotateRight) :: impl [] xs
        | x :: xs -> impl (x :: digits) xs
    in
    impl [] path

let (rows, cols, field, path) =
    let raw_input = Aoc.Common.read_input "input_22.txt" in

    let rec split_input lines acc found_empty = match lines with
        | x :: xs when found_empty -> (List.rev acc, x |> String.to_seq |> List.of_seq |> parse_path)
        | "" :: xs -> split_input xs acc true
        | x :: xs -> split_input xs (x :: acc) false
        | _ -> failwith "Invalid input"
    in

    let (field, path) = split_input raw_input [] false in

    let rows = List.length field in
    let cols = field |> List.map String.length |> List.fold_left (max) 0 in
    let arr = Array.make_matrix rows cols ' ' in
    field |> List.iteri (fun row line ->
        line |> String.iteri (fun col ch ->
            arr.(row).(col) <- ch
        )
    );
    (rows, cols, arr, path)

let start_col =
    let res = ref 0 in
    field.(0) |> Array.iteri (fun i ch ->
        if !res = 0 && ch = '.' then res := i);
    !res

let dirs = [|
    (0, 1);
    (1, 0);
    (0, -1);
    (-1, 0);
|]

type dir_name =
| L | R | U | D

let dir_by_name = function
| R -> 0
| D -> 1
| L -> 2
| U -> 3

let inv_dir_name = function
| U -> D
| D -> U
| L -> R
| R -> L

let dir_cnt = Array.length dirs

let safe_inc_mod x delta y = (x + delta + y) mod y

let move_pos (r, c) (dr, dc) = (
    safe_inc_mod r dr rows,
    safe_inc_mod c dc cols
)

let teleports =
    let res = Hashtbl.create 0 in

    (* `r*` and `c*` are 1-based *)
    let add_to_res ((r1, c1), move1, tel1) ((r2, c2), move2, tel2) =
        let (move1_dir, move2_dir) = (dir_by_name move1, dir_by_name move2) in
        let (tel1_dir, tel2_dir) = (dir_by_name tel1, dir_by_name tel2) in
        let (inv_tel1_dir, inv_tel2_dir) = (
            dir_by_name (inv_dir_name tel1),
            dir_by_name (inv_dir_name tel2)
        ) in

        let rec impl i pos1 pos2 =
            Hashtbl.add res (tel1_dir, pos1) (pos2, inv_tel2_dir);
            Hashtbl.add res (tel2_dir, pos2) (pos1, inv_tel1_dir);
            if i + 1 < 50
            then impl
                (i + 1)
                (move_pos pos1 dirs.(move1_dir))
                (move_pos pos2 dirs.(move2_dir))
        in
        impl 0 (r1 - 1, c1 - 1) (r2 - 1, c2 - 1)
    in

    (* 3 *)
    add_to_res
        ((100, 100), U, R)
        ((50, 150), L, D);

    (* 4 *)
    add_to_res
        ((101, 1), R, U)
        ((51, 51), D, L);

    (* 5 *)
    add_to_res
        ((200, 50), U, R)
        ((150, 100), L, D);

    (* 7 *)
    add_to_res
        ((1, 51), D, L)
        ((150, 1), U, L);

    (* 8 *)
    add_to_res
        ((1, 51), R, U)
        ((151, 1), D, L);

    (* 11 *)
    add_to_res
        ((1, 101), R, U)
        ((200, 1), R, D);

    (* 12 *)
    add_to_res
        ((1, 150), D, R)
        ((150, 100), U, R);

    res

let solve find_adj =
    let move ((row, col) as pos, dir_idx) act =
        match act with
            | Rotate RotateLeft ->
                let new_dir_idx = safe_inc_mod dir_idx (-1) dir_cnt in
                (pos, new_dir_idx)
            | Rotate RotateRight ->
                let new_dir_idx = safe_inc_mod dir_idx 1 dir_cnt in
                (pos, new_dir_idx)
            | Move cnt ->
                let rec step rem_cnt ((pos, dir_idx) as state) =
                    if rem_cnt = 0
                    then state
                    else
                        let next_state_cand = find_adj pos dir_idx in
                        let next_state = Option.value next_state_cand ~default:state in
                        step (rem_cnt - 1) next_state
                in
                step cnt (pos, dir_idx)
    in
    let ((final_row, final_col), dir_idx) = List.fold_left move ((0, start_col), 0) path
    in
    1000 * (final_row + 1) + 4 * (final_col + 1) + dir_idx

let solve_easy =
    let find_adj ((row, col) as start) dir_idx =
        let dir = dirs.(dir_idx) in
        let rec impl ((cand_r, cand_c) as cand) =
            match field.(cand_r).(cand_c) with
                | '#' -> None
                | '.' -> Some (cand, dir_idx)
                | ' ' -> impl (move_pos cand dir)
                | _ -> failwith "Oh no"
        in
        impl (move_pos start dir)
    in
    let ans = solve find_adj in
    Printf.printf "easy: %d\n" ans

let solve_hard =
    let find_adj ((row, col) as start) dir_idx =
        let dir = dirs.(dir_idx) in
        let ((next_r, next_c) as next_pos) = move_pos start dir in
        match field.(next_r).(next_c) with
        | ' ' ->
            let ((next_r, next_c), _) as next_state = Hashtbl.find teleports (dir_idx, start) in
            (match field.(next_r).(next_c) with
            | '#' -> None
            | '.' -> Some next_state
            | _ -> failwith "Whoa")
        | '#' -> None
        | '.' -> Some (next_pos, dir_idx)
        | _ -> failwith "Oh no"
    in
    let ans = solve find_adj in
    Printf.printf "hard: %d\n" ans
