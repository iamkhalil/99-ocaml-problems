let rec at (k: int) (my_list: 'a list): 'a option =
    match my_list with
    | [] -> None
    | _ :: _ when k < 1  -> None
    | x :: _ when k == 1 -> Some x
    | _ :: rest when k > 1 -> at (k - 1) rest
