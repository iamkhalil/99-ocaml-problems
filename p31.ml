let is_prime (nbr: int): bool =
    let rec is_prime' (nbr: int) (itr: int): bool =
        if itr > nbr / 2 then true
        else if nbr mod itr = 0 then false
        else is_prime' nbr (itr + 1)
    in match nbr with
    | 0 | 1 -> false
    | _ -> is_prime' nbr 2
