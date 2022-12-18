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
let profit rate time =
    assert (time <= minutes);
    rate * (minutes - time)

let is_bit_set n i = (n land (1 lsl i)) <> 0
let remove_bit n i = n land (lnot (1 lsl i))

let best_pressure =
    let name_arr = Array.of_list ("AA" :: good_valve_names) in
    let n = Array.length name_arr in
    let rate_arr = Array.init n (fun i ->
        let v = Hashtbl.find name_to_valve name_arr.(i) in
        v.Valve.flow_rate
    ) in
    let dist_arr = Array.init n (fun i ->
        Array.init n (fun j ->
            let iv = Hashtbl.find name_to_valve name_arr.(i) in
            let jv = Hashtbl.find name_to_valve name_arr.(j) in
            Hashtbl.find valve_dist (iv.Valve.name, jv.Valve.name)
        )
    )
    in
    let cache = Array.init n (fun _ ->
        Array.init (minutes + 1) (fun _ ->
            Array.init (Int.shift_left 1 n) (fun _ ->
                None
            )
        )
    )
    in
    let rec impl cur time left =
        match cache.(cur).(time).(left) with
        | Some v -> v
        | None ->
            let res = ref 0 in
            for i = 0 to n - 1 do
                if (i <> cur) && (is_bit_set left i)
                then
                begin
                    let to_go = dist_arr.(cur).(i) in
                    let next_time = time + to_go + 1 in
                    if next_time <= minutes
                    then
                        let pr = profit rate_arr.(i) (time + to_go) in
                        let sub_res = impl i next_time (remove_bit left i) in
                        let cand = pr + sub_res in
                        if cand > !res
                        then
                            res := cand
                end
            done;
            cache.(cur).(time).(left) <- Some !res;
            !res
    in
    impl 0 1 ((1 lsl n) - 2)

let solve_easy = Printf.printf "easy: %d\n" best_pressure
