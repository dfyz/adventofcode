module Minerals = struct
    type t = {
        ore: int;
        clay: int;
        obsidian: int;
        geode: int;
    }

    let zero = {
        ore = 0;
        clay = 0;
        obsidian = 0;
        geode = 0;
    }

    let from_ore x = {
        zero with ore = x;
    }

    let to_str x =
        Printf.sprintf "(ore=%d, clay=%d, obsidian=%d, geode=%d)" x.ore x.clay x.obsidian x.geode
end

let collect_minerals avail robots =
    {
        Minerals.ore = avail.Minerals.ore + robots.Minerals.ore;
        clay = avail.clay + robots.clay;
        obsidian = avail.obsidian + robots.obsidian;
        geode = avail.geode + robots.geode;
    }

let spend_minerals avail cost =
    assert (cost.Minerals.geode = 0);
    let res = {
        avail with
        Minerals.ore = avail.Minerals.ore - cost.ore;
        clay = avail.clay - cost.clay;
        obsidian = avail.obsidian - cost.obsidian;
    }
    in
    if res.ore >= 0 && res.clay >= 0 && res.obsidian >= 0
    then
        Some res
    else
        None

module Blueprint = struct
    type t = {
        id: int;
        ore: Minerals.t;
        clay: Minerals.t;
        obsidian: Minerals.t;
        geode: Minerals.t;
    }
end

let input =
    let lines = Aoc.Common.read_input "input_19.txt" in
    Aoc.Common.parse_lines_with_re
        {|Blueprint \(.*\): Each ore robot costs \(.*\) ore. Each clay robot costs \(.*\) ore. Each obsidian robot costs \(.*\) ore and \(.*\) clay. Each geode robot costs \(.*\) ore and \(.*\) obsidian.|}
        lines
        7
        (fun elems -> match List.map int_of_string elems with
        | [a; b; c; d; e; f; g] -> {
            Blueprint.id = a;
            ore = Minerals.from_ore b;
            clay = Minerals.from_ore c;
            obsidian = { Minerals.zero with ore = d; clay = e;};
            geode = { Minerals.zero with ore = f; obsidian = g; }
        }
        | _ -> failwith "Invalid line")

let minutes = 24

let blueprint_profit blueprint =
    let cache = Hashtbl.create 0 in
    let rec impl ((time, resources, robots) as key) =
    (* Printf.printf "time=%d, resources=%s, robots=%s\n" time (Minerals.to_str resources) (Minerals.to_str robots); *)
    match Hashtbl.find_opt cache key with
        | Some v -> v
        | None -> let computed =
            if time > minutes
            then
                resources.Minerals.geode
            else
                [
                    (Some resources, robots);
                    (spend_minerals resources blueprint.Blueprint.geode, {
                        robots with Minerals.geode = robots.Minerals.geode + 1;
                    });
                    (spend_minerals resources blueprint.obsidian, {
                        robots with obsidian = robots.obsidian + 1;
                    });
                    (spend_minerals resources blueprint.clay, {
                        robots with clay = robots.clay + 1;
                    });
                    (spend_minerals resources blueprint.ore, {
                        robots with ore = robots.ore + 1;
                    });
                ] |> List.filter_map (fun (new_res, new_robots) -> match new_res with
                    | Some res ->
                        let after_collection = collect_minerals res robots in
                        (*
                            wow
                            such heuristics
                            very machine learning
                        *)
                        if after_collection.ore > 15
                        then
                            None
                        else
                            Some (impl (time + 1, after_collection, new_robots))
                    | None -> None
                ) |> List.fold_left (max) 0
            in
            Hashtbl.add cache key computed;
            computed
    in
    impl (1, Minerals.zero, { Minerals.zero with ore = 1; })

let _ =
    let ans = ref 0 in
    input |> List.iteri (fun i bp ->
        let profit = blueprint_profit bp in
        Printf.printf "#%d: %d\n" (i + 1) profit;
        ans := !ans + profit * (i + 1);
        flush_all ()
    );
    Printf.printf "easy: %d\n" !ans
