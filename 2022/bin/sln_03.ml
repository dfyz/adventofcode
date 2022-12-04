module CS = Set.Make(Char)

let to_priority common_set =
    assert (CS.cardinal common_set = 1);
    let ch = CS.choose common_set in
    let by_off from base = (base + (int_of_char ch - int_of_char from)) in
    match ch with
    | 'a'..'z' -> by_off 'a' 1
    | 'A'..'Z' -> by_off 'A' 27
    | _ -> failwith "Invalid char"

let input = Aoc.Common.read_input "input_03.txt"

let to_cs x = CS.of_seq (String.to_seq x)

let easy =
    let easy_input =
        let split line =
            let half_len = String.length line / 2 in
            let left = String.sub line 0 half_len in
            let right = String.sub line half_len half_len in
            (left, right)
        in
        List.map split input
    in
    let score_easy (left, right) =
        let (left_set, right_set) = (to_cs left, to_cs right) in
        let common_set = CS.filter (fun x -> CS.mem x right_set) left_set in
        to_priority common_set
    in
    Aoc.Common.list_sum (List.map score_easy easy_input)

let hard =
    let score_group lines =
        let sets = List.map to_cs lines in
        let common_set = List.fold_left CS.inter (List.hd sets) (List.tl sets) in
        to_priority common_set
    in
    let rec score acc lst = match lst with
        | (a :: b :: c :: rest) -> score ((score_group [a; b; c]) :: acc) rest
        | [] -> Aoc.Common.list_sum acc
        | _ -> failwith "Invalid list length"
    in
    score [] input

let _ =
    Printf.printf "easy: %d\n" easy;
    Printf.printf "hard: %d\n" hard