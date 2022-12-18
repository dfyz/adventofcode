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

let exposed_sides (x, y, z) locs =
    let deltas = [
        (1, 0, 0);
        (-1, 0, 0);
        (0, 1, 0);
        (0, -1, 0);
        (0, 0, 1);
        (0, 0, -1);
    ] in
    deltas |> List.map (fun (dx, dy, dz) ->
        let adj = (x + dx, y + dy, z + dz) in
        if Locs.mem adj locs then 0 else 1
    ) |> List.fold_left (+) 0

let solve_easy =
    let ans = input |> Locs.to_seq |> List.of_seq
        |> List.map (fun cube -> exposed_sides cube input)
        |> List.fold_left (+) 0
    in
    Printf.printf "easy: %d\n" ans
