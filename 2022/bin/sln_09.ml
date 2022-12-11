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

let move_rope rope dir =
    let res = Array.copy rope in
    res.(0) <- move_head res.(0) dir;
    for i = 1 to (Array.length rope - 1) do
        res.(i) <- move_tail res.(i - 1) res.(i)
    done;
    res

let solve_rope rope_len =
    let rec make_move (dir, rem) locs rope =
        let cur_tail = rope.(Array.length rope - 1) in
        let new_locs = Locs.add cur_tail locs in
        if rem <= 0 then (new_locs, rope)
        else
            let new_rope = move_rope rope dir in
            make_move (dir, rem - 1) new_locs new_rope
    in
        let (all_locs, _) = List.fold_left
            (fun (locs, rope) line ->
                make_move line locs rope
            )
            (Locs.empty, Array.make rope_len (0, 0))
            input
    in
    Locs.cardinal all_locs

let _ =
    Printf.printf "easy: %d\n" (solve_rope 2);
    Printf.printf "hard: %d\n" (solve_rope 10)