let rec last_two (my_list: 'a list): ('a * 'a) option =
    match my_list with
    | [] | [_] -> None
    | [x; y] -> Some (x, y)
    | _ :: rest -> last_two rest
