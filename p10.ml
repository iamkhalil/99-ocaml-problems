let encode (my_list: 'a list): (int * 'a) list =
    let rec encode_tail (my_list: 'a list) (count: int) (x: 'a): (int * 'a) list =
        match my_list with
        | [] -> [(count, x)]
        | y :: rest -> if y = x
                       then encode_tail rest (count + 1) x
                       else (count, x) :: encode_tail rest 1 y
    in match my_list with
    | [] -> []
    | x :: rest -> encode_tail rest 1 x


let () =
    let my_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in
    let result = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] in
    Printf.printf "%b\n" (encode my_list = result)
