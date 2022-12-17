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

let name_to_valve = input
    |> List.map (fun v -> (v.Valve.name, v))
    |> List.to_seq
    |> Hashtbl.of_seq

let minutes = 30

(* What do we get if the valve is opened for all t > time? *)
let profit valve time =
    assert (time <= minutes);
    valve.Valve.flow_rate * (minutes - time)

let best_pressure =
    let cache = Hashtbl.create 0 in
    let rec impl cur_name cur_valves time =
        let cache_key = (cur_name, cur_valves, time) in
        match Hashtbl.find_opt cache cache_key with
        | Some v -> v
        | None ->
            let computed_val =
                if time > minutes
                then
                    0
                else
                    let cur = Hashtbl.find name_to_valve cur_name in
                    let if_no_open_max = cur.adj |> List.map (
                        fun next_name -> impl next_name cur_valves (time + 1)
                    ) |> List.fold_left (max) 0
                    in
                    if cur.flow_rate = 0 || ValveSet.mem cur_name cur_valves
                    then
                        if_no_open_max
                    else
                        let open_profit = profit cur time in
                        (* Printf.printf "profit from %s at time %d: %d\n" cur_name time open_profit; *)
                        let if_open =
                            impl
                                cur_name
                                (ValveSet.add cur_name cur_valves)
                                (time + 1)
                        in
                        max if_no_open_max (open_profit + if_open)
            in
            Hashtbl.add cache cache_key computed_val;
            computed_val
    in
    impl "AA" ValveSet.empty 1

let solve_easy = Printf.printf "easy: %d\n" best_pressure
