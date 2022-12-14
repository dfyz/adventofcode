module Locs = Aoc.Common.Locs

let input =
    let line_to_pairs line =
        let tokens = String.split_on_char ' ' line in
        List.filter_map (fun x -> match x with
            | "->" -> None
            | cs ->
                match String.split_on_char ',' cs with
                    | [x; y] -> Some (int_of_string x, int_of_string y)
                    | _ -> failwith "Invalid input"
        ) tokens
    in
    let raw_input = Aoc.Common.read_input "input_14.txt" in
    List.map line_to_pairs raw_input

let trace_path path =
    let rec trace_segment ((x1, y1) as src) ((x2, y2) as dst) =
            let move_coord src_coord dst_coord =
                let delta = dst_coord - src_coord in
                let abs_delta = abs delta in
                if abs_delta = 0
                    then dst_coord
                    else
                        src_coord + (delta / abs_delta)
            in
            let rest =
                if src = dst
                then Locs.empty
                else trace_segment (move_coord x1 x2, move_coord y1 y2) dst
            in
            Locs.add src rest
    in
    let rec impl = function
        | src :: dst :: rest ->
            let seg = trace_segment src dst in
            (dst :: rest) |> impl |> Locs.union seg
        | _ -> Locs.empty
    in
    impl path

let traced_input =
    let traces = List.map trace_path input in
    List.fold_left Locs.union Locs.empty traces

let (sand_x, sand_y) as sand_pos = (500, 0)
let max_y = traced_input |> Locs.to_seq |> Seq.map snd |> Seq.fold_left (max) sand_y

let drop_sand trace =
    let rec impl ((x, y) as pos) trace =
        if y > max_y
            then None
            else
                let next_y = y + 1 in
                let next_xs = [x; x - 1; x + 1] in
                let cands = List.map (fun x -> (x, next_y)) next_xs in
                match List.find_opt (fun x -> not (Locs.mem x trace)) cands with
                    | Some next_pos -> impl next_pos trace
                    | None -> Some (Locs.add pos trace)
    in
    impl sand_pos trace

let compute_sand =
    let rec impl trace =
        match drop_sand trace with
            | None -> Locs.diff trace traced_input
            | Some next_trace -> impl next_trace
    in
    impl traced_input

let easy =
    Printf.printf "easy: %d\n" (Locs.cardinal compute_sand)
