let sumEven list =
    let rec recSumEven total recList = 
        match recList with
        |[] -> total
        |hd::tl ->
            if hd % 2 = 0 && hd > 0 then recSumEven (total + hd) tl else recSumEven total tl
    recSumEven 0 list
        

//Helper function for isValidTable
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

let getClosestPair list =
    let rec recGetClosestPair coor dis recList =
        match recList with
        |[] -> coor
        |hd::tl ->
            let (x,y) = hd
            let calDis = sqrt(x*x + y*y)
            if calDis < dis then recGetClosestPair (x,y) calDis tl else recGetClosestPair coor dis tl
    recGetClosestPair (infinity, infinity) infinity list