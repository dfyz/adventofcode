let input_name = "sample_input_11.txt"

module type Num = sig
    type t

    val add: t -> t -> t
    val mul: t -> t -> t
    val div: t -> int -> t
    val modulus: t -> int -> int
    val of_int_and_mods: int list -> int -> t
end

module RegularNum: Num = struct
    type t = int

    let add = (+)
    let mul = ( * )
    let div = (/)
    let modulus = (mod)
    let of_int_and_mods _ x = x
end

module ModNum: Num = struct
    type t = {
        mod_vals: int list;
        mods: int list;
    }

    let apply_op op num1 num2 =
        assert (num1.mods = num2.mods);
        let zipped = List.combine num1.mod_vals num2.mod_vals |> List.combine num1.mods in {
            mod_vals = List.map op zipped;
            mods = num1.mods;        
        }

    let add = apply_op (fun (m, (a, b)) -> (a + b) mod m)
    let mul = apply_op (fun (m, (a, b)) -> (a * b) mod m)

    let div _ _ = failwith "Division is not supported"

    let modulus num m =
        List.combine num.mod_vals num.mods
            |> List.find (fun (_, mm) -> m = mm)
            |> fst

    let of_int_and_mods mods x = {
        mod_vals = List.map (fun m -> x mod m) mods;
        mods;
    }
end

module Monkey(N: Num) = struct
    type num = N.t

    type t = {
        mutable items: num Queue.t;
        op: num -> num;
        div_test: int;
        true_monkey: int;
        false_monkey: int;
        mutable inspections: int;
    }

    let parse_input () =
        let last_int s =
            s |> String.split_on_char ' ' |> List.rev |> List.hd |> int_of_string
        in

        let lines = Aoc.Common.read_input input_name in
        let all_mods = List.filter_map (fun line ->
            match (line |> String.trim |> String.split_on_char ' ') with
                | "Test:" :: _ -> Some (last_int line)
                | _ -> None
        ) lines
        in
        let rec parse = function
            | [] -> []
            | "" :: rest -> parse rest
            | _ :: l2 :: l3 :: l4 :: l5 :: l6 :: rest ->
                let items = match String.split_on_char ':' l2 with
                    | [_; str_items] ->
                        str_items
                            |> String.split_on_char ','
                            |> List.map String.trim
                            |> List.map int_of_string
                            |> List.map (N.of_int_and_mods all_mods)
                            |> List.to_seq
                            |> Queue.of_seq
                    | _ -> failwith "Invalid items"
                in

                let parse_op s = match s |> String.split_on_char ' ' |> List.rev with
                    | operand2 :: op_name :: operand1 :: _ ->
                        let op = match op_name with
                            | "+" -> N.add
                            | "*" -> N.mul
                            | _ -> failwith "Invalid operation name"
                        in
                        let eval_operand o =
                            fun x -> match o with
                                | "old" -> x
                                | imm -> int_of_string imm |> N.of_int_and_mods all_mods
                        in
                        fun x ->
                            let lhs = x |> eval_operand operand1 in
                            let rhs = x |> eval_operand operand2 in
                            op lhs rhs
                    | _ -> failwith "Invalid operation"
                in

                let monkey = {
                    items = items;
                    op = parse_op l3;
                    div_test = last_int l4;
                    true_monkey = last_int l5;
                    false_monkey = last_int l6;
                    inspections = 0;
                }
                in
                monkey :: parse rest
            | _ -> failwith "Invalid input"
        in
        lines |> parse |> Array.of_list

    let run_round monkeys div_by =
        let run_monkey m =
            Queue.iter (fun item ->
                let new_item = N.div (m.op item) div_by in
                let new_monkey =
                    if N.modulus new_item m.div_test = 0
                        then m.true_monkey
                        else m.false_monkey
                in
                Queue.push new_item monkeys.(new_monkey).items;
                m.inspections <- m.inspections + 1
            ) m.items;
            Queue.clear m.items
        in
        Array.iter run_monkey monkeys

    let run_rounds round_count div_by =
        let monkeys = parse_input () in
        let rec impl = function
            | x when x > 0 ->
                run_round monkeys div_by;
                impl (x - 1)
            | _ -> monkeys
        in
        impl round_count

    let monkey_score monkeys =
        match
            monkeys
            |> Array.to_list
            |> List.map (fun x -> x.inspections)
            |> List.sort (Fun.flip compare)
        with
            | best1 :: best2 :: _ -> best1 * best2
            | _ -> failwith "Not enough monkeys"
end

let solve_easy =
    let module M = Monkey(RegularNum) in
    let monkeys = M.run_rounds 20 3 in
    let ans = M.monkey_score monkeys in
    Printf.printf "easy: %d\n" ans
