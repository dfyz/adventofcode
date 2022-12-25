let input =
    Aoc.Common.read_input "input_20.txt"
        |> List.mapi (fun i x -> (i, int_of_string x))
        |> Array.of_list

let find_pos nums num_id =
    let rec impl i =
        match nums.(i) with
        | (j, delta) when num_id = j -> (i, j, delta)
        | _ -> impl (i + 1)
    in
    impl 0

let safe_mod x y =
    let ans = x mod y in
    if ans < 0
    then ans + y
    else ans

let move nums num_id =
    let n = Array.length nums in
    let (x, num_id, delta) = find_pos nums num_id in
    let n1 = n - 1 in
    let aux =
        let lhs = Array.sub nums 0 x in
        let rhs = Array.sub nums (x + 1) (n1 - x) in
        Array.append lhs rhs
    in
    let new_pos =
        if delta >= 0
        then safe_mod (x + delta) n1
        else safe_mod ((safe_mod (x - 1 + delta) n1) + 1) n
    in
    let lhs = Array.sub aux 0 new_pos in
    let rhs = Array.sub aux new_pos (n1 - new_pos) in
    Array.concat [lhs; [| (num_id, delta) |]; rhs]

let to_deltas x = x |> Array.map snd |> Array.to_list

let mix nums =
    let rec impl num_id acc =
        if num_id >= (Array.length nums)
        then acc
        else impl (num_id + 1) (move acc num_id)
    in
    impl 0 nums

let solve_easy =
    let rec find_zero_id i = match input.(i) with
    | (num_id, 0) -> num_id
    | _ -> find_zero_id (i + 1)
    in
    let zero_id = find_zero_id 0 in
    let mixed = mix input in
    let (zero_pos, _, _) = find_pos mixed zero_id in
    let n = Array.length mixed in
    let get_num delta = mixed.((zero_pos + delta) mod n) |> snd in
    let ans = get_num 1000 + get_num 2000 + get_num 3000 in
    Printf.printf "easy: %d\n" ans
