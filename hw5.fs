let rec sumEven list =
    match list with
    |[] -> 0
    |hd::tl -> 
        if hd % 2 = 0 then hd + sumEven tl else sumEven tl

let rec inList list value =
    match list with
    |[] -> false
    |hd::tl ->
        if value = hd then true else inList tl value

let rec isValidTable constraints list =
    match constraints with
    |[] -> true
    |hd::tl -> 
        let (name1, name2) = hd 
        if inList list name1 && inList list name2 then false else isValidTable tl list

let rec getClosestPair list =
    match list with
    |[] -> (infinity, infinity)
    |hd::tl -> 