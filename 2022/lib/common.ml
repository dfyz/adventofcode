let get_file_name base_name =
    Filename.concat (Filename.concat Filename.parent_dir_name "data") base_name

let read_input base_name =
    let inp = open_in (get_file_name base_name) in
    let rec read acc = match (input_line inp) with
        | exception End_of_file -> List.rev acc
        | line -> read (line :: acc) in
    read []

let parse_lines_with_re line_re lines group_count extractor =
    let re = Str.regexp line_re in
    let parse_line line =
        assert (Str.string_match re line 0);
        let rec get_groups idx =
            if idx > group_count
            then []
            else (Str.matched_group idx line) :: get_groups (idx + 1)
        in
        get_groups 1 |> extractor
    in
    List.map parse_line lines

let list_sum = List.fold_left (+) 0

module Locs = Set.Make(struct
    type t = int * int
    let compare = compare
end)
