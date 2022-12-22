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

let eval human_val =
    let rec impl monkey =
        match Hashtbl.find input monkey with
        | Yell num -> (match (monkey, human_val) with
            | ("humn", Some x) -> x
            | _ -> num)
        | Combine (monkey1, op, monkey2) ->
            let res1 = impl monkey1 in
            let res2 = impl monkey2 in
            match op with
                | Add -> res1 + res2
                | Sub -> res1 - res2
                | Mul -> res1 * res2
                | Div -> assert (res1 mod res2 = 0); res1 / res2
    in
    impl "root"

let solve_easy =
    Printf.printf "easy: %d\n" (eval None)