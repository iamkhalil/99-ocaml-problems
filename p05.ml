let rev (my_list: 'a list): 'a list =
    let rec reverse (my_list: 'a list) (res: 'a list): 'a list =
        match my_list with
        | [] -> res
        | x :: rest -> reverse rest (x :: res)
    in reverse my_list []
