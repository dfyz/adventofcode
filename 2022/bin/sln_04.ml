type segment = {
    first: int;
    last: int;
}

let contains (smol, large) =
    (large.first <= smol.first) && (large.last >= smol.last)

let check_easy (a, b) = contains (a, b) || contains (b, a)
let check_hard (a, b) =
    not (a.first > b.last || b.first > a.last)

let input =
    let parse_segment seg = match (String.split_on_char '-' seg) with
        | f :: l :: [] -> {
            first = int_of_string f;
            last = int_of_string l;
        }
        | _ -> failwith "Invalid segment"
    in
    let parse_line line =
        match (String.split_on_char ',' line) with
        | a :: b :: [] -> (parse_segment a, parse_segment b)
        | _ -> failwith "Invalid line"
    in
    List.map parse_line (Aoc.Common.read_input "input_04.txt")

let easy = List.length (List.find_all check_easy input)
let hard = List.length (List.find_all check_hard input)

let _ =
    Printf.printf "easy: %d\n" easy;
    Printf.printf "hard: %d\n" hard