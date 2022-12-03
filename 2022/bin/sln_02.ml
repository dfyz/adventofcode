type move = Rock | Paper | Scissors
type spec = X | Y | Z

let input =
    let to_move = function
        | "A" -> Rock | "B" -> Paper | "C" -> Scissors
        | unk -> failwith (Printf.sprintf "Unknown move: %s" unk)
    in
    let to_spec = function
        | "X" -> X | "Y" -> Y | "Z" -> Z
        | unk -> failwith (Printf.sprintf "Unknown spec: %s" unk)
    in
    let line_to_input line = match (String.split_on_char ' ' line) with
        | a :: b :: _ -> ((to_move a), (to_spec b))
        | _ -> failwith "Expected exactly two moves"
    in
    List.map line_to_input (Aoc.Common.read_input "input_02.txt")

let improve = function | Rock -> Paper | Paper -> Scissors | Scissors -> Rock
let weaken = function | Rock -> Scissors | Paper -> Rock | Scissors -> Paper

let round_score round =
    let outcome_score = match round with
        | (x, y) when x = y -> 3
        | (x, y) when (improve x) = y -> 6
        | _ -> 0
    in
    let move_score = match (snd round) with
        | Rock -> 1 | Paper -> 2 | Scissors -> 3
    in
    outcome_score + move_score

let score_all finalize_round =
    let round_score_full rnd = round_score (finalize_round rnd) in
    List.fold_left (+) 0 (List.map round_score_full input)

let easy =
    let finalize_round (a, b) = (a, match b with | X -> Rock | Y -> Paper | Z -> Scissors) in
    score_all finalize_round

let hard =
    let finalize_round (a, b) = (a, match b with | X -> weaken a | Y -> a | Z -> improve a) in
    score_all finalize_round

let _ =
    Printf.printf "easy: %d\n" easy;
    Printf.printf "hard: %d\n" hard