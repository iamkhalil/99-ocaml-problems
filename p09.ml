let pack (my_list: 'a list): 'a list list =
    let rec pack_tail (my_list: 'a list) (current_list: 'a list) (nested_list: 'a list list) : 'a list list =
        match my_list with
        | [] -> []
        | [x] -> (x :: current_list) :: nested_list
        | x1 :: x2 :: rest -> if x1 = x2
                              then pack_tail (x2 :: rest) (x1 :: current_list) nested_list
                              else pack_tail (x2 :: rest) [] ((x1 :: current_list) :: nested_list)
    in List.rev (pack_tail my_list [] [])


(* Entry point *)
let () =
    let my_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in
    let result = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]] in
    Printf.printf "%b\n" (pack my_list = result)
