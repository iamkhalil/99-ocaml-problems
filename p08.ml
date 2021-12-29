let compress (my_list: 'a list) : 'a list =
    let rec compress_rec (my_list: 'a list) (x: 'a) =
        match my_list with
        | [] -> []
        | elem :: rest -> if elem = x
                          then compress_rec rest elem
                          else elem :: compress_rec rest elem
    in match my_list with
    | [] -> []
    | x :: rest -> x :: compress_rec rest x


(* Entry point *)
let () =
    assert (compress [] = []);
    assert (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"])
