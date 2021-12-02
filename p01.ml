let rec last (my_list: 'a list): 'a option =
    match my_list with
    | [] -> None
    | [x] -> Some x
    | _ :: rest -> last rest
