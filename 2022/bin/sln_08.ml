module Locs = Aoc.Common.Locs

let (rows, cols, input) =
    let raw_input = Aoc.Common.read_input "input_08.txt" in
    let rows = List.length raw_input in
    let cols = raw_input |> List.hd |> String.length in
    let res = Array.make_matrix rows cols 0 in
    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            res.(r).(c) <- String.sub (List.nth raw_input r) c 1 |> int_of_string
        done
    done;
    (rows, cols, res)

let in_bounds r c = r >= 0 && c >= 0 && r < rows && c < cols

let add_locs start delta =
    let rec impl (r, c) (dr, dc) max_val acc =
        if not (in_bounds r c)
        then
            acc
        else
            let cur = input.(r).(c) in
            let new_acc = if cur > max_val then (Locs.add (r, c) acc) else acc in
            impl ((r + dr), (c + dc)) (dr, dc) (max cur max_val) new_acc
    in
    impl start delta (-1) Locs.empty

let solve_easy =
    let rec check_coord coord max_coord acc coord_to_params =
        if coord >= max_coord
        then
            acc
        else
            let ((start1, delta1), (start2, delta2)) = coord_to_params coord in
            let acc1 = add_locs start1 delta1 in
            let acc2 = add_locs start2 delta2 in
            let new_acc = Locs.union acc (Locs.union acc1 acc2) in
            check_coord (coord + 1) max_coord new_acc coord_to_params
    in
    let row_locs = check_coord 0 rows Locs.empty (fun z ->
        ((z, 0       ), (0,  1)),
        ((z, cols - 1), (0, -1))
    ) in
    let col_locs = check_coord 0 cols Locs.empty (fun z ->
        ((0,        z), ( 1, 0)),
        ((rows - 1, z), (-1, 0))
    ) in
    let all_locs = Locs.union row_locs col_locs in
    Printf.printf "easy: %d\n" (Locs.cardinal all_locs)

let viewing_distance (orig_r, orig_c) (dr, dc) =
    let orig = input.(orig_r).(orig_c) in
    let rec impl = function
        | (r, c) when (in_bounds r c) ->
            let cur = input.(r).(c) in
            let rest = if cur < orig then impl (r + dr, c + dc) else 0 in
            1 + rest
        | _ -> 0
    in
    impl (orig_r + dr, orig_c + dc)

let scenic_score pos =
    let deltas = [
        ( 1,  0);
        (-1,  0);
        ( 0,  1);
        ( 0, -1)
    ] in
    let distances = List.map (fun delta -> viewing_distance pos delta) deltas in
    List.fold_left ( * ) 1 distances

let solve_hard =
    let res = ref 0 in
    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            res := max !res (scenic_score (r, c))
        done
    done;
    Printf.printf "hard: %d\n" !res