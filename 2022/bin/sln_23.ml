module Locs = Aoc.Common.Locs

let input =
    let raw_input = Aoc.Common.read_input "input_23.txt" in
    raw_input |> List.mapi (fun row line ->
        line |> String.to_seq |> List.of_seq |> List.mapi (fun col ch ->
            if ch = '#' then Some (row, col) else None
        ) |> List.filter_map (fun x -> x)
    ) |> List.concat |> Locs.of_list

let bounding_box locs =
    Locs.fold (fun (r, c) (min_row, max_row, min_col, max_col) ->
            (
                min min_row r,
                max max_row r,
                min min_col c,
                max max_col c
            )
        ) locs (Int.max_int, Int.min_int, Int.max_int, Int.min_int)

let print_elves locs =
    let (min_row, max_row, min_col, max_col) = bounding_box locs in
    for r = min_row to max_row do
        for c = min_col to max_col do
            let ch =
                match Locs.find_opt (r, c) locs with
                | Some _ -> '#'
                | None -> '.'
            in
            print_char ch
        done;
        print_newline ();
    done

let dirs = [|
    [(-1, 0); (-1, 1); (-1, -1)];
    [(1, 0); (1, 1); (1, -1)];
    [(0, -1); (-1, -1); (1, -1)];
    [(0, 1); (-1, 1); (1, 1)];
|]

let dir_cnt = Array.length dirs

let all_dirs = dirs |> Array.to_list |> List.concat |> Locs.of_list

let move (r, c) (dr, dc) = (r + dr, c + dc)

let move_elves locs dir_idx =
    let proposals = Hashtbl.create 0 in
    Locs.iter (fun ((r, c) as elf) ->
        let has_elf dir = Locs.mem (move elf dir) locs in
        let rec check off =
            if off < dir_cnt
            then begin
                let cur_dirs = dirs.((dir_idx + off) mod dir_cnt) in
                if cur_dirs |> List.exists has_elf
                then check (off + 1)
                else
                    let cand = move elf (List.hd cur_dirs) in
                    match Hashtbl.find_opt proposals cand with
                    | None -> Hashtbl.add proposals cand (Some elf)
                    | Some _ -> Hashtbl.replace proposals cand None
            end
        in
        if all_dirs |> Locs.exists has_elf
        then check 0
    ) locs;
    Hashtbl.fold (fun dst src cur_locs ->
        match src with
        | Some src -> cur_locs |> Locs.remove src |> Locs.add dst
        | None -> cur_locs
    ) proposals locs

let solve_easy =
    let rec do_round round locs =
        if round >= 10
        then locs
        else
            let moved = move_elves locs (round mod dir_cnt) in
            do_round (round + 1) moved
    in
    let final = do_round 0 input in
    let (min_row, max_row, min_col, max_col) = bounding_box final in
    let area = (max_row - min_row + 1) * (max_col - min_col + 1) in
    let ans = area - (Locs.cardinal final) in
    Printf.printf "easy: %d\n" ans
