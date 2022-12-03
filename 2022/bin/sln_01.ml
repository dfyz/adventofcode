let data =
    let input = open_in (Aoc.Common.get_file_name "input_01.txt") in
    let finalize res = List.rev (List.map List.rev res) in
    let rec parse res acc =
        match (input_line input) with
        | exception End_of_file -> acc :: res
        | "" -> parse (acc :: res) []
        | line -> parse res ((int_of_string line) :: acc) in
    finalize (parse [] [])

let sum_list elf = List.fold_left (+) 0 elf
let calories = List.map sum_list data

let easy = List.fold_left max 0 calories
let hard = match List.rev (List.sort compare calories) with
    | (a :: b :: c :: _) -> a + b + c
    | _ -> raise (invalid_arg "Check your input")

let _ =
    Printf.printf "easy: %d\n" easy;
    Printf.printf "hard: %d\n" hard