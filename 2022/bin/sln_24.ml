module Locs = Map.Make(struct
    type t = int * int
    let compare = compare
end)

let locs_of_list raw_locs =
    raw_locs |> List.fold_left (fun locs (((r, c) as pos), ch) ->
        Locs.update pos (function
        | Some prev -> Some (ch :: prev)
        | None -> Some [ch]
        ) locs
    ) Locs.empty

let (rows, cols, input) =
    let raw_input = Aoc.Common.read_input "input_24.txt" in
    let (rows, cols) = (List.length raw_input - 2, (raw_input |> List.hd |> String.length) - 2) in

    let raw_locs = raw_input |> List.mapi (fun r line ->
        line |> String.to_seq |> List.of_seq |> List.mapi (fun c ch -> match ch with
        | '#' | '.' -> None
        | '>' | '<' | '^' | 'v' -> Some ((r, c), ch)
        | _ -> failwith "???"
        ) |> List.filter_map (fun x -> x)
    ) |> List.concat
    in
    (rows, cols, locs_of_list raw_locs)

let start_pos = (0, 1)
let finish_pos = (rows + 1, cols)

let print_locs locs =
    for r = 0 to rows + 1 do
        for c = 0 to cols + 1 do
            let to_print =
                if r = 0 || c = 0 || r = rows + 1 || c = cols + 1
                then begin
                    if (r, c) = start_pos || (r, c) = finish_pos
                    then '.'
                    else '#'
                end
                else match Locs.find_opt (r, c) locs with
                | Some [ch] -> ch
                | Some chs -> String.get (string_of_int (List.length chs)) 0
                | None -> '.'
            in
            print_char to_print
        done;
        print_newline ()
    done

let move_locs locs =
    let move (r, c) ch =
        let (dr, dc) = match ch with
        | '>' -> (0, 1)
        | '<' -> (0, -1)
        | '^' -> (-1, 0)
        | 'v' -> (1, 0)
        | _ -> failwith "Wat"
        in
        let wrap coord max_coord =
            if coord = 0
            then max_coord
            else
                if coord > max_coord
                then 1
                else coord
        in
        (
            wrap (r + dr) rows,
            wrap (c + dc) cols
        )
    in
    locs |> Locs.to_seq |> List.of_seq |> List.concat_map (fun (pos, chs) ->
        chs |> List.map (fun ch -> (move pos ch, ch))
    ) |> locs_of_list

type leg =
| Initial
| Intermediate
| Final

let next_leg cur_leg next_pos =
    match (cur_leg, next_pos) with
    | (Initial, x) when x = finish_pos -> Intermediate
    | (Intermediate, x) when x = start_pos -> Final
    | _ -> cur_leg

let solve is_easy =
    let locs_cache = Hashtbl.create 0 in
    Hashtbl.add locs_cache 0 input;
    let time_to_locs t =
        match Hashtbl.find_opt locs_cache t with
        | Some v -> v
        | None ->
            let prev = Hashtbl.find locs_cache (t - 1) in
            let res = move_locs prev in
            Hashtbl.add locs_cache t res;
            res
    in
    let q = Queue.create () in
    let dist = Hashtbl.create 0 in

    let push_elem ((_, elem_dist, _) as elem) =
        Queue.push elem q;
        Hashtbl.add dist elem elem_dist
    in

    let rec bfs () =
        match Queue.take_opt q with
        | None -> failwith "No path to finish"
        | Some (Final, t, pos) when pos = finish_pos -> t
        | Some (leg, t, (r, c)) ->
            let next_field = time_to_locs (t + 1) in
            let can_move ((_, next_time, ((nr, nc) as next_pos)) as next_elem) =
                not (Locs.mem next_pos next_field)
                && not (Hashtbl.mem dist next_elem)
                && (
                    ((nr > 0) && (nc > 0) && (nr <= rows) && (nc <= cols))
                    || next_pos = finish_pos || next_pos = start_pos
                )
            in
            let adj = [
                (r, c);
                (r + 1, c);
                (r - 1, c);
                (r, c + 1);
                (r, c - 1);
            ]
                |> List.map (fun pos ->
                    (next_leg leg pos, t + 1, pos)
                )
                |> List.filter can_move
            in
            List.iter push_elem adj;
            bfs ()

    in

    push_elem ((if is_easy then Final else Initial), 0, start_pos);
    bfs ()

let _ =
    Printf.printf "easy: %d\n" (solve true);
    Printf.printf "hard: %d\n" (solve false)
