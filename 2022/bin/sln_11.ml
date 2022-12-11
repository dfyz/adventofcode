module Monkey = struct
    type t = {
        mutable items: int Queue.t;
        op: int -> int;
        div_test: int;
        true_monkey: int;
        false_monkey: int;
        mutable inspections: int;
    }

    let items_to_str m =
        m.items
            |> Queue.to_seq
            |> List.of_seq
            |> List.map string_of_int
            |> String.concat ", "
end


let input () =
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
                        |> List.to_seq
                        |> Queue.of_seq
                | _ -> failwith "Invalid items"
            in

            let parse_op s = match s |> String.split_on_char ' ' |> List.rev with
                | operand2 :: op_name :: operand1 :: _ ->
                    let op = match op_name with
                        | "+" -> (+)
                        | "*" -> ( * )
                        | _ -> failwith "Invalid operation name"
                    in
                    let eval_operand o =
                        fun x -> match o with
                            | "old" -> x
                            | imm -> int_of_string imm
                    in
                    fun x ->
                        let lhs = x |> eval_operand operand1 in
                        let rhs = x |> eval_operand operand2 in
                        op lhs rhs
                | _ -> failwith "Invalid operation"
            in

            let last_int s =
                s |> String.split_on_char ' ' |> List.rev |> List.hd |> int_of_string
            in

            let monkey = {
                Monkey.items = items;
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
    Aoc.Common.read_input "input_11.txt" |> parse |> Array.of_list

let run_round monkeys div_by =
    let run_monkey m =
        Queue.iter (fun item ->
            let new_item = (m.Monkey.op item) / div_by in
            let new_monkey =
                if new_item mod m.div_test = 0
                    then m.true_monkey
                    else m.false_monkey
            in
            Queue.push new_item monkeys.(new_monkey).Monkey.items;
            m.inspections <- m.inspections + 1
        ) m.items;
        Queue.clear m.items
    in
    Array.iter run_monkey monkeys

let run_rounds round_count div_by =
    let monkeys = input () in
    let rec impl = function
        | x when x > 0 ->
            run_round monkeys div_by;
            impl (x - 1)
        | _ -> monkeys
    in
    impl round_count

let solve_easy =
    let monkeys = run_rounds 20 3 in
    match
        monkeys
        |> Array.to_list
        |> List.map (fun x -> x.Monkey.inspections)
        |> List.sort (Fun.flip compare)
    with
        | best1 :: best2 :: _ -> Printf.printf "easy: %d\n" (best1 * best2)
        | _ -> failwith "Not enough monkeys"
