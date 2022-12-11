module Locs = Aoc.Common.Locs

let input =
    Aoc.Common.read_input "input_09.txt" |> List.map (fun line ->
        match (String.split_on_char ' ' line) with
            | [dir; steps] -> (dir, int_of_string steps)
            | _ -> failwith "invalid input"
    )

let move_head (hx, hy) dir =
    let (dx, dy) = match dir with
        | "L" -> (0, -1)
        | "R" -> (0, 1)
        | "U" -> (1, 0)
        | "D" -> (-1, 0)
        | _ -> failwith "Invalid directions"
    in
    (hx + dx, hy + dy)

let move_tail (hx, hy) ((tx, ty) as tail_pos) =
    let (dx, dy) = (hx - tx, hy - ty) in
    match (abs dx, abs dy) with
        | (abs_dx, abs_dy) when (abs_dx <= 1 && abs_dy <= 1) -> tail_pos
        | (abs_dx, abs_dy) when (abs_dx <= 2 || abs_dy <= 2) ->
            let move delta abs_delta = match abs_delta with
                | 0 -> 0
                | 1 -> delta
                | 2 -> delta / 2
                | _ -> failwith "uh oh"
            in
            (tx + move dx abs_dx, ty + move dy abs_dy)
        | _ -> failwith "invalid head/tail configuration"

let solve_easy =
    let rec make_move (dir, rem) locs head_pos tail_pos =
        let new_locs = Locs.add tail_pos locs in    
        if rem <= 0 then (new_locs, head_pos, tail_pos)
        else
            let new_head = move_head head_pos dir in
            let new_tail = move_tail new_head tail_pos in
            make_move (dir, rem - 1) new_locs new_head new_tail
    in
        let (all_locs, _, _) = List.fold_left (fun (locs, head_pos, tail_pos) line ->
            make_move line locs head_pos tail_pos
        ) (Locs.empty, (0, 0), (0, 0)) input
    in
    Printf.printf "easy: %d\n" (Locs.cardinal all_locs)