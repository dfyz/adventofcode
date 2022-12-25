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

let local_geodes time res robots =
    let time_left = minutes - time + 1 in
    res.Minerals.geode + robots.Minerals.geode * time_left

let try_buy_robot time resources cost robots robo_delta =
    let rec impl time resources =
        if time > minutes
        then None
        else begin
            match spend_minerals resources cost with
            | Some after_robot ->
                let new_robots = collect_minerals robots robo_delta in
                let new_resources = collect_minerals after_robot robots in
                Some (time + 1, new_resources, new_robots)
            | None ->
                let new_resources = collect_minerals resources robots in
                impl (time + 1) new_resources
        end
    in impl time resources

let blueprint_profit blueprint =
    let cache = Hashtbl.create 0 in
    let rec impl ((time, resources, robots) as key) =
    (* Printf.printf "time=%d, resources=%s, robots=%s\n" time (Minerals.to_str resources) (Minerals.to_str robots); *)
    match Hashtbl.find_opt cache key with
        | Some v -> v
        | None -> let computed =
            let local_profit = local_geodes time resources robots in
            if time > minutes
            then
                local_profit
            else
                let after_robot_profit = [
                    (blueprint.Blueprint.geode, {
                        Minerals.zero with geode = 1;
                    });
                    (blueprint.Blueprint.obsidian, {
                        Minerals.zero with obsidian = 1;
                    });
                    (blueprint.Blueprint.clay, {
                        Minerals.zero with clay = 1;
                    });
                    (blueprint.Blueprint.ore, {
                        Minerals.zero with ore = 1;
                    });
                ] |> List.filter_map (fun (cost, robo_delta) ->
                    try_buy_robot time resources cost robots robo_delta
                ) |> List.map impl |> List.fold_left (max) 0
                in
                max local_profit after_robot_profit
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
