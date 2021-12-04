(* Non-tail recursive solution *)
let rec length (my_list: 'a list): int =
    match my_list with
    | [] -> 0
    | _ :: rest -> 1 + length rest


(* Tail recursive solution *)
let rec length_tail (my_list: 'a list) (result: int): int =
    match my_list with
    | [] -> result
    | _ :: rest -> length_tail rest (result + 1)

(* Tail recursive solution, but in an elegant way *)
let length_tail_wrapper (my_list: 'a list): int =
    let rec length_tail_rec (my_list: 'a list) (result: int): int =
        match my_list with
        | [] -> result
        | _ :: rest -> length_tail_rec rest (result + 1)
    in length_tail_rec my_list 0
