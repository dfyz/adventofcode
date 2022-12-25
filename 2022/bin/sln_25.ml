let input = Aoc.Common.read_input "input_25.txt"

let snafu_to_decimal x =
    let rec impl pow digits = match digits with
    | [] -> 0
    | ch :: rest ->
        let cur = match ch with
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | '-' -> -1
        | '=' -> -2
        | _ -> failwith "NOOOO"
        in
        cur * pow + (impl (pow * 5) rest)
    in
    x |> String.to_seq |> List.of_seq |> List.rev |> impl 1

let decimal_sum = input |> List.map snafu_to_decimal |> List.fold_left (+) 0

let rec decimal_to_snafu x =
    let rec base5 x =
        if x = 0
        then []
        else
            let digit = x mod 5 in
            let rest = base5 (x / 5) in
            digit :: rest
    in

    let rec to_snafu digits carry = match digits with
    | [] -> if carry then [1] else []
    | d :: rest ->
        let d = d + (if carry then 1 else 0) in
        let (carry, d) = if d > 2 then (true, d - 5) else (false, d) in
        d :: (to_snafu rest carry)
    in

    to_snafu (base5 x) false |> List.map (function
        | -2 -> '='
        | -1 -> '-'
        | 0 -> '0'
        | 1 -> '1'
        | 2 -> '2'
        | _ -> failwith "???"
    ) |> List.rev |> List.to_seq |> String.of_seq

let _ =
    Printf.printf "easy: %s\n" (decimal_to_snafu decimal_sum);
    Printf.printf "hard: *\n"