module CS = Set.Make(Char)

let input = Aoc.Common.read_input "input_06.txt"

let has_no_repeats s =
    (String.length s) = (s |> String.to_seq |> CS.of_seq |> CS.cardinal)

let find_repeat_start line window_size =
    let rec find_impl idx =
        if idx + window_size <= (String.length line)
        then
            if has_no_repeats (String.sub line idx window_size)
            then
                idx + window_size
            else
                find_impl (idx + 1)
        else
            failwith "Unexpected end of string"
    in
    find_impl 0

let actual_input = List.hd input
let easy = find_repeat_start actual_input 4
let hard = find_repeat_start actual_input 14

let _ =
    Printf.printf "%d\n" easy;
    Printf.printf "%d\n" hard