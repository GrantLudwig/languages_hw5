let rec sumEven list =
    match list with
    |[] -> 0
    |hd::tl -> 
        if hd % 2 = 0 then hd + sumEven tl else sumEven tl

let rec inList list value1 value2 =
    match list with
    |[] -> (false, false)
    |hd::tl ->
        let (boolV1, boolV2) = inList tl value1 value2
        if value1 = hd then (true,boolV2) elif value2 = hd then (boolV1, true) else (boolV1, boolV2)

let rec isValidTable constraints list =
    match constraints with
    |[] -> true
    |hd::tl -> 
        let (name1, name2) = hd 
        if inList list name1 name2 = (true, true) then false else isValidTable tl list

let rec getClosestPair list =
    match list with
    |[] -> (infinity, infinity)
    |hd::tl -> 

//Test
//TODO DELETE!!!!!

//isValid
//Larson
let x = [("Eric", "Mark"); ("Anna", "Maya"); ("Beth", "Hope")];;
isValidTable x ["Eric"; "Anna"; "Beth"];;
isValidTable x ["Greg"; "Eric"; "John"; "Anna"; "Beth"];;
isValidTable x ["Hope"; "Eric"; "Anna"; "Beth"];;
isValidTable x ["Mark"; "Beth"; "Eric"; "Anna"];;
//Mine
let bigList = [(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200)];;
isValidTable bigList [1..199];;
isValidTable bigList [200..5000];;