type file = {
    file_name: string;
    file_size: int;
}

type directory = {
    dir_parent: directory option;
    dir_name: string;
    mutable dir_children: (string, directory) Hashtbl.t;
    mutable dir_files: file list;
}

let input = Aoc.Common.read_input "input_07.txt"

let root_dir =
    let empty_dir parent name = {
        dir_parent = parent;
        dir_name = name;
        dir_children = Hashtbl.create 0;
        dir_files = [];
    } in
    let root = empty_dir None "/" in
    let rec parse lines cur_dir = match lines with
        | [] -> ()
        | (line :: rest) ->
            let next_dir = match (String.split_on_char ' ' line) with
                | ["$"; "cd"; ".."] -> Option.get cur_dir.dir_parent
                | ["$"; "cd"; child_name] -> Hashtbl.find cur_dir.dir_children child_name
                | ["$"; "ls"] -> cur_dir (* just ignore the `ls` lines *)
                | ["dir"; dir_name] ->
                    let child_dir = empty_dir (Some cur_dir) dir_name in
                    Hashtbl.add cur_dir.dir_children dir_name child_dir;
                    cur_dir
                | [file_size; file_name] ->
                    let new_file = {
                        file_name;
                        file_size = int_of_string file_size;
                    } in
                    cur_dir.dir_files <- new_file :: cur_dir.dir_files;
                    cur_dir
                | _ -> failwith "invalid line"
            in
            parse rest next_dir
    in
    parse (List.tl input) root;
    root

let disk_space = 70000000
let need_space = 30000000

let solve =
    let rec traverse_easy cur_dir =
        let file_sizes = List.map (fun x -> x.file_size) cur_dir.dir_files in
        let file_total = List.fold_left (+) 0 file_sizes in
        let sub_answers = List.map traverse_easy (cur_dir.dir_children |> Hashtbl.to_seq_values |> List.of_seq) in
        let (sub_total, sub_easy_total, sub_totals) = List.fold_left
            (fun (a1, b1, c1) (a2, b2, c2) -> (
                a1 + a2,
                b1 + b2,
                List.append c1 c2
            ))
            (0, 0, [])
            sub_answers
        in
        let total = sub_total + file_total in
        (
            total,
            sub_easy_total + (if total <= 100000 then total else 0),
            List.sort compare (total :: sub_totals)
        )
    in
    let (full_total, easy_total, full_totals) = traverse_easy root_dir in
    Printf.printf "easy: %d\n" easy_total;
    let free_space = disk_space - full_total in
    assert (free_space < need_space);
    let to_delete = List.find ((<=) (need_space - free_space)) full_totals in
    Printf.printf "hard: %d\n" to_delete
