let input =
    let raw_input = Aoc.Common.read_input "input_10.txt" in
    List.map
        (fun line -> match (String.split_on_char ' ' line) with
            | ["noop"] -> None
            | ["addx"; num] -> Some (int_of_string num)
            | _ -> failwith "Invalid input"
        )
        raw_input

let solve_easy =
    let rec simulate insns cycle value acc =
        let update_acc cycle acc =
            acc +
                (if (cycle - 20) mod 40 = 0 && cycle <= 220
                then cycle * value
                else 0)
        in
        match insns with
        | [] -> acc
        | (insn :: rest) ->
            match insn with
                | Some delta ->
                    let acc1 = update_acc cycle acc in
                    let acc2 = update_acc (cycle + 1) acc1 in
                    simulate rest (cycle + 2) (value + delta) acc2
                | None ->
                    let acc1 = update_acc cycle acc in
                    simulate rest (cycle + 1) value acc1
    in
    let ans = simulate input 1 1 0 in
    Printf.printf "easy: %d\n" ans

