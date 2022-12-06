let input = Aoc.Common.read_input "input_05.txt"

let input_to_crates () =
    let n = ((String.length (List.hd input)) + 1) / 4 in
    let crates = Array.init n (fun _ -> []) in
    let update_crates line =
        for i = 0 to (n - 1) do
            let ch = line.[1 + 4*i] in
            if ch <> ' ' then crates.(i) <- (ch :: crates.(i))
        done
    in
    let rec iter_lines lines = match lines with
        | (line :: rest) when String.contains line '[' ->
            update_crates line;
            iter_lines rest
        | _ ->
            for i = 0 to (n - 1) do
                crates.(i) <- List.rev crates.(i)
            done
    in
    iter_lines input;
    crates

let make_moves rev =
    let crates = input_to_crates () in
    let update_crates line =
        let tokens = String.split_on_char ' ' line in
        match tokens with
            | "move" :: cnt :: "from" :: src :: "to" :: dst :: [] ->
                let (cnt, src, dst) = (int_of_string cnt, (int_of_string src) - 1, (int_of_string dst) - 1) in
                let rec take_n n acc = match n with
                    | 0 ->
                        let to_append = if rev then (List.rev acc) else acc in
                        crates.(dst) <- List.append to_append crates.(dst)
                    | x ->
                        let old_src = crates.(src) in
                        crates.(src) <- List.tl old_src;
                        take_n (x - 1) (List.hd old_src :: acc)
                in
                take_n cnt []
            | _ -> failwith "Invalid move"
    in
    let rec iter_lines lines = match lines with
        | (line :: rest) ->
            if (String.length line > 0) && (line.[0] = 'm') then
                update_crates line;
            iter_lines rest
        | _ -> ()
    in
    iter_lines input;
    String.of_seq (Array.to_seq (Array.map List.hd crates))

let _ =
    Printf.printf "easy: %s\n" (make_moves false);
    Printf.printf "hard: %s\n" (make_moves true)