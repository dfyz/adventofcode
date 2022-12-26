let input = Aoc.Common.read_input "input_17.txt" |> List.hd

module Locs = Aoc.Common.Locs

(* x goes →, y goes ↑, x = 0 is the left wall, y = 0 is the floor *)
let max_x = 7

let max_y locs = locs |> Locs.to_seq |> List.of_seq |> List.map snd |> List.fold_left (max) 0
let start_pos locs = (3, 4 + max_y locs)

let print_chamber locs =
    let max_y = max_y locs in
    for y = max_y downto 1 do
        for x = 1 to max_x do
            let ch = if Locs.mem (x, y) locs then '#' else '.' in
            print_char ch;
        done;
        print_newline ()
    done

let move_rocks (dx, dy) =
    Locs.map (fun (x, y) -> (x + dx, y + dy))

let rocks = [
    [(0, 0); (1, 0); (2, 0); (3, 0)];
    [(1, 0); (0, 1); (1, 1); (2, 1); (1, 2)];
    [(0, 0); (1, 0); (2, 0); (2, 1); (2, 2)];
    [(0, 0); (0, 1); (0, 2); (0, 3)];
    [(0, 0); (1, 0); (0, 1); (1, 1)]
] |> List.map Locs.of_list

let drop_rock chamber rock jet_idx =
    let is_valid_figure figure =
        Locs.disjoint chamber figure &&
        Locs.for_all (fun (x, y) -> x > 0 && x <= max_x && y > 0) figure
    in
    let rec impl figure jet_idx =
        let jet = match String.get input jet_idx with
            | '>' -> (1, 0)
            | '<' -> (-1, 0)
            | _ -> failwith "Invalid jet"
        in
        let figure_after_jet =
            let cand = move_rocks jet figure in
            if is_valid_figure cand
            then
                cand
            else
                figure
        in
        let figure_after_fall = move_rocks (0, -1) figure_after_jet in
        let next_jet_idx = (jet_idx + 1) mod (String.length input) in
        if is_valid_figure figure_after_fall
        then
            impl figure_after_fall next_jet_idx
        else
            (next_jet_idx, Locs.union figure_after_jet chamber)
    in
    let figure = move_rocks (start_pos chamber) rock in
    impl figure jet_idx

let drop_n_rocks n =
    let rec impl n rock_idx jet_idx chamber =
        if n = 0
        then
            []
        else
            let rock = List.nth rocks (rock_idx mod (List.length rocks)) in
            let (next_jet_idx, next_chamber) = drop_rock chamber rock jet_idx in
            let delta = (max_y next_chamber - max_y chamber) in
            delta :: impl (n - 1) (rock_idx + 1) next_jet_idx next_chamber
    in
    impl n 0 0 Locs.empty

let rocks_5k = drop_n_rocks 5000

let rec take_n n lst =
    let rec impl cnt acc lst =
        if cnt >= n
            then (List.rev acc, lst)
            else impl (cnt + 1) ((List.hd lst) :: acc) (List.tl lst)
    in
    impl 0 [] lst

let sum_list = List.fold_left (+) 0

let solve_easy =
    let ans = take_n 2022 rocks_5k |> fst |> sum_list in
    Printf.printf "easy: %d\n" ans

let solve_hard =
    (* These values were found by eyeballing `rocks_5k` in Sublime Text lmao. *)
    let prefix_len = 632 in
    let cycle_len = 1705 in

    let target = 1000000000000 in

    let target_without_prefix_len = target - prefix_len in
    let cycle_prefix_len = target_without_prefix_len mod cycle_len in

    let (prefix, suffix) = take_n prefix_len rocks_5k in
    let (cycle, _) = take_n cycle_len suffix in
    let (cycle_prefix, _) = take_n cycle_prefix_len cycle in

    let ans = (target_without_prefix_len / cycle_len * (sum_list cycle)) +
              (sum_list prefix) +
              (sum_list cycle_prefix) in
    Printf.printf "hard: %d\n" ans
