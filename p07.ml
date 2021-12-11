type 'a node =
    | One of 'a
    | Many of 'a node list

let flatten (nodes: 'a node list): 'a list =
    let rec flatten_rec (nodes: 'a node list) (result: 'a list) : 'a list =
        match nodes with
        | [] -> result
        | One x :: rest -> flatten_rec rest (x :: result)
        | Many xs :: rest -> flatten_rec rest (flatten_rec xs result)
    in List.rev (flatten_rec nodes [])
