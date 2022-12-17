type sensor = {
    pos: int * int;
    beacon: int * int;
}

let input =
    let raw_input = Aoc.Common.read_input "input_15.txt" in
    Aoc.Common.parse_lines_with_re
        {|Sensor at x=\(.+\), y=\(.+\): closest beacon is at x=\(.+\), y=\(.+\)|}
        raw_input
        4
        (fun lst -> match List.map int_of_string lst with
            | [a; b; c; d] -> {
                pos = (a, b);
                beacon = (c, d);
            }
            | _ -> failwith "Invalid input"
        )

let sensor_dist {
    pos = (x1, y1);
    beacon = (x2, y2);
} = abs (x1 - x2) + abs (y1 - y2)

let x_spans at_y =
    let x_spans_for_sensor sensor =
        let (x_center, y_center) = sensor.pos in
        let max_dist = sensor_dist sensor in
        let y_delta = abs (y_center - at_y) in
        if y_delta > max_dist
            then None
            else let x_delta = max_dist - y_delta in
                 Some (x_center - x_delta, x_center + x_delta)
    in
    input
        |> List.filter_map x_spans_for_sensor
        |> List.sort compare

let bound = 2000000

(* Assumes the spans don't have any gaps in the easy problem *)
let solve_easy =
    let spans = x_spans bound in
    let (min_start, max_end) = List.fold_left (fun (s1, e1) (s2, e2) ->
        (min s1 s2, max e1 e2)
    ) (List.hd spans) (List.tl spans)
    in
    Printf.printf "easy: %d\n" (max_end - min_start)

let solve_hard =
    let bound = 2 * bound in
    let rec find_gap max_end sensors = match sensors with
        | (s, e) :: rest ->
            let cand = max_end + 1 in
            if max_end + 2 = s && cand >= 0 && cand <= bound
                then Some cand
                else find_gap (max e max_end) rest
        | _ -> None
    in
    let rec search y =
        if y > bound
        then failwith "No gap found"
        else
            let spans = x_spans y in
            match find_gap (spans |> List.hd |> snd) (List.tl spans) with
                | Some gap -> (gap, y)
                | None -> search (y + 1)
    in
    let (x, y) = search 0 in
    let ans = x * 4000000 + y in
    Printf.printf "hard: %d\n" ans
