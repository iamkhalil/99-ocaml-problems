let rec length (my_list: 'a list): int =
    match my_list with
    | [] -> 0
    | _ :: rest -> 1 + length rest
