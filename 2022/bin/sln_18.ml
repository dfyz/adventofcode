module Locs = Set.Make(struct
    type t = int * int * int
    let compare = compare
end)

let input =
    let raw_input = Aoc.Common.read_input "input_18.txt" in
    raw_input |> List.map (
        fun line -> match String.split_on_char ',' line with
            | [x; y; z] -> (int_of_string x, int_of_string y, int_of_string z)
            | _ -> failwith "Invalid cube"
    ) |> Locs.of_list

let deltas = [
    (1, 0, 0);
    (-1, 0, 0);
    (0, 1, 0);
    (0, -1, 0);
    (0, 0, 1);
    (0, 0, -1);
]

let get_adj (x, y, z) =
    deltas |> List.map (fun (dx, dy, dz) ->
        (x + dx, y + dy, z + dz)
    )

let count_adj is_good_adj =
    input
        |> Locs.to_seq |> List.of_seq
        |> List.concat_map (fun pos ->
            get_adj pos |> List.map (fun adj ->
                if is_good_adj adj then 1 else 0
            )
        )
        |> List.fold_left (+) 0

let solve_easy =
    let ans = count_adj (fun adj -> not (Locs.mem adj input)) in
    Printf.printf "easy: %d\n" ans

let hull =
    let (min_bound, max_bound) = (-1, 25) in
    let visited = Hashtbl.create 0 in
    let rec dfs cur =
        let adj = get_adj cur in
        let good_adj = adj |> List.filter (fun ((x, y, z) as next) ->
            (x >= min_bound && y >= min_bound && z >= min_bound) &&
            (x <= max_bound && y <= max_bound && z <= max_bound) &&
            not (Locs.mem next input)
        ) in
        Hashtbl.add visited cur true;
        good_adj |> List.iter (fun next ->
            match Hashtbl.find_opt visited next with
            | None -> dfs next
            | _ -> ()
        )
    in
    dfs (min_bound, min_bound, min_bound);
    visited |> Hashtbl.to_seq_keys |> Locs.of_seq

let solve_hard =
    let ans = count_adj (fun adj -> Locs.mem adj hull) in
    Printf.printf "hard: %d\n" ans
