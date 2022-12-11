let input =
    let raw_input = Aoc.Common.read_input "input_10.txt" in
    List.map
        (fun line -> match (String.split_on_char ' ' line) with
            | ["noop"] -> None
            | ["addx"; num] -> Some (int_of_string num)
            | _ -> failwith "Invalid input"
        )
        raw_input

let simulate callback =
    let rec impl insns cycle value =
        match insns with
        | [] -> ()
        | (insn :: rest) ->
            match insn with
                | Some delta ->
                    callback cycle value;
                    callback (cycle + 1) value;
                    impl rest (cycle + 2) (value + delta)
                | None ->
                    callback cycle value;
                    impl rest (cycle + 1) value
    in
    impl input 1 1

let solve_easy =
    let res = ref 0 in
    let callback cycle value =
        if (cycle - 20) mod 40 = 0 && cycle <= 220
        then
            let strength = cycle * value in
            res := !res + strength
    in
    simulate callback;
    Printf.printf "easy: %d\n" !res

let rows = 6
let cols = 40

let solve_hard =
    let res = Array.make_matrix rows cols '.' in
    let callback cycle value =
        let zc = cycle - 1 in
        let (draw_r, draw_c) = (zc / cols, zc mod cols) in
        if List.mem draw_c [value - 1; value; value + 1]
        then res.(draw_r).(draw_c) <- '#'
    in
    simulate callback;
    print_endline "hard: ";
    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            print_char res.(r).(c)
        done;
        print_newline ()
    done