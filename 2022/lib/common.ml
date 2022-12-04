let get_file_name base_name =
    Filename.concat (Filename.concat Filename.parent_dir_name "data") base_name

let read_input base_name =
    let inp = open_in (get_file_name base_name) in
    let rec read acc = match (input_line inp) with
        | exception End_of_file -> List.rev acc
        | line -> read (line :: acc) in
    read []

let list_sum = List.fold_left (+) 0