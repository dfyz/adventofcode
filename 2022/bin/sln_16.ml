module ValveSet = Set.Make(String)

module Valve = struct
    type t = {
        name: string;
        flow_rate: int;
        adj: string list;
    }
end

let input =
    Aoc.Common.parse_lines_with_re
        {|Valve \(.*\) has flow rate=\(.*\); tunnels? leads? to valves? \(.*\)|}
        (Aoc.Common.read_input "input_16.txt")
        3
        (function
            | [a; b; c] -> {
                Valve.name = a;
                flow_rate = int_of_string b;
                adj = c |> String.split_on_char ',' |> List.map String.trim
            }
            | _ -> failwith "Invalid input"
        )

let good_valve_names = input
    |> List.filter (fun v -> v.Valve.flow_rate > 0)
    |> List.map (fun v -> v.Valve.name)

let name_to_valve = input
    |> List.map (fun v -> (v.Valve.name, v))
    |> List.to_seq
    |> Hashtbl.of_seq

let valve_dist =
    let dist = Hashtbl.create 0 in
    input |> List.iter (fun src ->
        Hashtbl.add dist (src.Valve.name, src.Valve.name) 0;
        src.Valve.adj |> List.iter (fun dst ->
            Hashtbl.add dist (src.name, dst) 1
        )
    );
    input |> List.iter (fun kv ->
        let k = kv.Valve.name in
        input |> List.iter (fun iv ->
            let i = iv.Valve.name in
            input |> List.iter (fun jv ->
                let j = jv.Valve.name in
                match (Hashtbl.find_opt dist (i, k), Hashtbl.find_opt dist (k, j)) with
                | (Some dist_ik, Some dist_kj) ->
                    let new_dist = dist_ik + dist_kj in
                    begin
                        match Hashtbl.find_opt dist (i, j) with
                        | Some old_dist when new_dist < old_dist ->
                            Hashtbl.replace dist (i, j) new_dist
                        | None ->
                            Hashtbl.add dist (i, j) new_dist
                        | _ -> ()
                    end
                | _ -> ()
            )
        )
    );
    dist

let minutes = 30

(* What do we get if the valve is opened for all t > time? *)
let profit valve time =
    assert (time <= minutes);
    valve.Valve.flow_rate * (minutes - time)

let best_pressure =
    let cache = Hashtbl.create 0 in
    let rec impl cur time left =
        let cache_key = (cur, time, left) in
        match Hashtbl.find_opt cache cache_key with
        | Some v -> v
        | None ->
            let computed_val =
                left |> ValveSet.to_seq |> List.of_seq |> List.filter_map (function next ->
                    let to_go = Hashtbl.find valve_dist (cur, next) in
                    let next_time = time + to_go + 1 in
                    if next = cur || next_time > minutes
                    then
                        None
                    else
                        let pr = profit (Hashtbl.find name_to_valve next) (time + to_go) in
                        let sub_res = impl next next_time (ValveSet.remove next left) in
                        Some (pr + sub_res)
                ) |> List.fold_left (max) 0
            in
            Hashtbl.add cache cache_key computed_val;
            computed_val
    in
    impl "AA" 1 (ValveSet.of_list good_valve_names)

let solve_easy = Printf.printf "easy: %d\n" best_pressure
