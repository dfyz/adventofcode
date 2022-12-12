let raw_input = Aoc.Common.read_input "input_12.txt"

let rows = List.length raw_input
let cols = raw_input |> List.hd |> String.length

let input =
    let res = Array.make_matrix rows cols ' ' in
    List.iteri (fun r row ->
        String.iteri (fun c ch -> res.(r).(c) <- ch) row
    ) raw_input;
    res


let can_move (from_r, from_c) (to_r, to_c) =
    if to_r < 0 || to_c < 0 || to_r >= rows || to_c >= cols
    then false
    else
        let src = input.(from_r).(from_c) in
        let dst = input.(to_r).(to_c) in
        let rec to_level = function
            | 'S' -> to_level 'a'
            | 'E' -> to_level 'z'
            | x -> int_of_char x in
        (to_level dst - to_level src) <= 1

let find_pos ch =
    let rec impl r c =
        if input.(r).(c) = ch
        then (r, c)
        else match (r, c) with
            | (r, c) when r = rows - 1 && c = cols - 1 -> failwith "Char not found"
            | (_, c) when c < cols - 1 -> impl r (c + 1)
            | _ -> impl (r + 1) 0
    in
    impl 0 0

let start_pos = find_pos 'S'
let end_pos = find_pos 'E'

let solve_easy =
    let q = Queue.create () in
    let dist = Hashtbl.create 0 in

    let rec bfs q =
        match Queue.take_opt q with
            | None -> failwith "Queue exhausted"
            | Some ((r, c) as src) ->
                let cur_dist = Hashtbl.find dist src in
                if src = end_pos
                    then cur_dist
                    else
                        let adj = List.filter (fun dst ->
                            can_move src dst && not (Hashtbl.mem dist dst)
                        ) [
                            (r + 1, c);
                            (r - 1, c);
                            (r, c + 1);
                            (r, c - 1)
                        ] in
                        List.iter (fun dst ->
                            Hashtbl.add dist dst (1 + cur_dist);
                            Queue.push dst q
                        ) adj;
                        bfs q
    in

    Queue.push start_pos q;
    Hashtbl.add dist start_pos 0;
    Printf.printf "easy: %d\n" (bfs q)
