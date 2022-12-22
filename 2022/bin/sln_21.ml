type op_type =
    | Add
    | Sub
    | Mul
    | Div

type job_type =
    | Yell of int
    | Combine of string * op_type * string

type instruction = {
    monkey: string;
    job: job_type;
}

let input =
    let instructions = Hashtbl.create 0 in
    Aoc.Common.read_input "input_21.txt"
        |> List.iter (fun line -> match String.split_on_char ':' line with
            | [monkey; expr] ->
                let expr = String.trim expr in
                let job = match String.split_on_char ' ' expr with
                    | [a; b; c] ->
                        let op = match b with
                            | "+" -> Add
                            | "-" -> Sub
                            | "*" -> Mul
                            | "/" -> Div
                            | _ -> failwith "Invalid operation"
                        in
                        Combine (a, op, c)
                    | [num] -> Yell (int_of_string num)
                    | _ -> failwith "Invalid job"
                in
                Hashtbl.add instructions monkey job
            | _ -> failwith "Invalid line"
        );
    instructions

let safe_div x y = assert (x mod y = 0); x / y

let eval_op res1 op res2 = match op with
    | Add -> res1 + res2
    | Sub -> res1 - res2
    | Mul -> res1 * res2
    | Div -> safe_div res1 res2

let eval human_val =
    let rec impl monkey =
        match Hashtbl.find input monkey with
        | Yell num -> (match (monkey, human_val) with
            | ("humn", Some x) -> x
            | _ -> num)
        | Combine (monkey1, op, monkey2) ->
            let res1 = impl monkey1 in
            let res2 = impl monkey2 in
            eval_op res1 op res2
    in
    impl "root"

let solve_easy =
    Printf.printf "easy: %d\n" (eval None)

type eval_tree =
    | Const of int
    | Placeholder
    | Op of eval_tree * op_type * eval_tree

let rec calc_eval_tree monkey =
    if monkey = "humn"
    then
        Placeholder
    else
        match Hashtbl.find input monkey with
            | Yell num -> Const num
            | Combine (monkey1, op, monkey2) ->
                let res1 = calc_eval_tree monkey1 in
                let res2 = calc_eval_tree monkey2 in
                match (res1, res2) with
                    | (Const a, Const b) -> Const (eval_op a op b)
                    | (a, b) -> Op (a, op, b)

let solve_hard =
    let rec solve tree target = match tree with
        | Placeholder -> target
        | _ ->
            let (new_tree, x, op, x_on_left) = match tree with
                | Op (Const x, op, y) -> (y, x, op, true)
                | Op (y, op, Const x) -> (y, x, op, false)
                | _ -> failwith "Oh noes"
            in
            let new_target = match op with
                | Add -> target - x
                | Sub -> (if x_on_left then (-) else (+)) x target
                | Mul -> safe_div target x
                | Div -> (if x_on_left then safe_div else ( * )) x target
            in
            solve new_tree new_target
    in
    let ans = match calc_eval_tree "root" with
        | Op (tree, Add, Const target) -> solve tree target
        | _ -> failwith "Uh oh"
    in
    Printf.printf "hard: %d\n" ans
