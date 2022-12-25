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

let blueprint_profit blueprint minutes is_easy =
    let local_geodes time res robots =
        let time_left = minutes - time + 1 in
        res.Minerals.geode + robots.Minerals.geode * time_left
    in

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
    in

    let cache = Hashtbl.create 0 in
    let rec impl ((time, resources, robots) as key) =
    match Hashtbl.find_opt cache key with
        | Some v -> v
        | None -> let computed =
            let local_profit = local_geodes time resources robots in
            if time > minutes
            then
                local_profit
            else
                let pre_cands = [
                    (blueprint.Blueprint.geode, {
                        Minerals.zero with geode = 1;
                    });
                    (blueprint.Blueprint.obsidian, {
                        Minerals.zero with obsidian = 1;
                    });
                ]
                in
                let cands_with_clay =
                    pre_cands @
                    (if is_easy || robots.Minerals.clay < 9
                    then
                        [(blueprint.Blueprint.clay, {
                            Minerals.zero with clay = 1;
                        })]
                    else [])
                in
                let cands =
                    cands_with_clay @
                    (if is_easy || robots.Minerals.ore < 5
                    then
                        [(blueprint.Blueprint.ore, {
                            Minerals.zero with ore = 1;
                        })]
                    else [])
                in
                let after_robot_profit = cands |> List.filter_map (fun (cost, robo_delta) ->
                    try_buy_robot time resources cost robots robo_delta
                ) |> List.map impl |> List.fold_left (max) 0
                in
                max local_profit after_robot_profit
            in
            Hashtbl.add cache key computed;
            computed
    in
    impl (1, Minerals.zero, { Minerals.zero with ore = 1; })

let solve_easy =
    let minutes = 24 in
    let ans = input |> List.mapi (fun i bp ->
        let profit = blueprint_profit bp minutes true
        in
        profit * (i + 1)
    ) |> List.fold_left (+) 0
    in
    Printf.printf "easy: %d\n" ans

let solve_hard =
    let minutes = 32 in
    let ans = match input with
    | a :: b :: c :: _ ->
        [a; b; c] |> List.map (fun bp ->
        blueprint_profit bp minutes false
    ) |> List.fold_left ( * ) 1
    | _ -> failwith "Huh"
    in
    Printf.printf "hard: %d\n" ans
