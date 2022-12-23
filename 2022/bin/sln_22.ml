type rotation = | L | R

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
            (parse_digits digits) :: (if x = 'L' then Rotate L else Rotate R) :: impl [] xs
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

let dir_cnt = Array.length dirs

let safe_inc_mod x delta y = (x + delta + y) mod y

let solve_easy =
    let find_adj ((row, col) as start) (dr, dc) =
        let move_cand (cand_r, cand_c) = (
            safe_inc_mod cand_r dr rows,
            safe_inc_mod cand_c dc cols
        )
        in
        let rec impl ((cand_r, cand_c) as cand) =
            match field.(cand_r).(cand_c) with
                | '#' -> None
                | '.' -> Some cand
                | ' ' -> impl (move_cand cand)
                | _ -> failwith "Oh no"
        in
        impl (move_cand start)
    in
    let move ((row, col) as pos, dir_idx) act =
        match act with
            | Rotate L ->
                let new_dir_idx = safe_inc_mod dir_idx (-1) dir_cnt in
                (pos, new_dir_idx)
            | Rotate R ->
                let new_dir_idx = safe_inc_mod dir_idx 1 dir_cnt in
                (pos, new_dir_idx)
            | Move cnt ->
                let dir = dirs.(dir_idx) in
                let rec step rem_cnt pos =
                    if rem_cnt = 0
                    then pos
                    else
                        let next_pos_cand = find_adj pos dir in
                        let next_pos = Option.value next_pos_cand ~default:pos in
                        step (rem_cnt - 1) next_pos
                in
                (step cnt pos, dir_idx)
    in
    let ((final_row, final_col), dir_idx) = List.fold_left move ((0, start_col), 0) path
    in
    let ans = 1000 * (final_row + 1) + 4 * (final_col + 1) + dir_idx
    in
    Printf.printf "easy: %d\n" ans
