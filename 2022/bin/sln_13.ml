open Aoc.Data_13

let input =
    let parse s =
        let lexed = Lexing.from_string s in
        Aoc.Parser_13.parse_packet Aoc.Lexer_13.lex_packet lexed
    in
    let rec impl = function
        | [] -> []
        | "" :: rest -> impl rest
        | a :: b :: rest -> (parse a, parse b) :: impl rest
        | _ -> failwith "Invalid input"
    in
    let raw_input = Aoc.Common.read_input "input_13.txt" in
    impl raw_input

let rec in_order = function
    | (IntP a, IntP b) ->
    begin
        match compare a b with
        | x when x < 0 -> Some true
        | x when x > 0 -> Some false
        | x            -> assert (x = 0); None
    end
    | (ListP a, ListP b) ->
    begin
        match (a, b) with
        | ([], []) -> None
        | ([], _)  -> Some true
        | (_, [])  -> Some false
        | (a :: a_rest, b :: b_rest) ->
        begin
            match in_order (a, b) with
            | (Some _) as sub_res -> sub_res
            | None -> in_order (ListP a_rest, ListP b_rest)
        end
    end
    | (((IntP _) as a), b) -> in_order (ListP [a], b)
    | (a, ((IntP _) as b)) -> in_order (a, ListP [b])

let solve_easy =
    let rec impl idx pairs = match pairs with
        | [] -> 0
        | p :: rest ->
            let sub_res = impl (idx + 1) rest in
            let res = match in_order p with
                | Some true -> idx
                | Some false -> 0
                | None -> failwith "Undefined order in part 1"
            in
            sub_res + res
    in
    let ans = impl 1 input in
    Printf.printf "easy: %d\n" ans

let solve_hard =
    let flat_input = List.fold_left (fun res (a, b) ->
        a :: b :: res
    ) [] input
    in
    let dividers = List.map (fun x -> ListP [ListP [IntP x]]) [2; 6] in
    let to_sort = List.append flat_input dividers in
    let sorted = List.sort (fun a b -> match in_order (a, b) with
        | Some true -> -1
        | Some false -> 1
        | None -> failwith "Undefined order in part 2"
    ) to_sort in
    let pre_ans = List.mapi (fun idx x ->
        if List.mem x dividers
            then Some (idx + 1)
            else None
    ) sorted
    in
    let ans = List.fold_left (fun res x -> match x with
        | Some idx -> res * idx
        | None -> res
    ) 1 pre_ans
    in
    Printf.printf "hard: %d\n" ans
