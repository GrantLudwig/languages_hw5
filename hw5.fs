let sumEven list =
    let rec recSumEven total recList = 
        match recList with
        |[] -> total
        |hd::tl ->
            if hd % 2 = 0 then recSumEven (total + hd) tl else recSumEven total tl
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

//Test
//----------------
//TODO DELETE!!!!!
//----------------

//sumEven
//Larson
sumEven [1; 2; 1; 2; 1; 2];; //6
sumEven [1..10];; //30
sumEven [1; 3; 5; 7; 9];; //0
//Mine
sumEven [0; 1];; //0
sumEven [-2];; //-2
sumEven [-4; -1; -4; -3; 2; 0; 1; 3; 5; 6; 7; 8; 10; 11; 2; -1; 0; -90; -4; 5];; //-74

//isValid
//Larson
let x = [("Eric", "Mark"); ("Anna", "Maya"); ("Beth", "Hope")];;
isValidTable x ["Eric"; "Anna"; "Beth"];; //true
isValidTable x ["Greg"; "Eric"; "John"; "Anna"; "Beth"];; //true
isValidTable x ["Hope"; "Eric"; "Anna"; "Beth"];; //false
isValidTable x ["Mark"; "Beth"; "Eric"; "Anna"];; //false
//Mine
let bigList = [(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200);(1, 200)];;
isValidTable bigList [1..199];; //true
isValidTable bigList [200..5000];; //true
isValidTable bigList [-5000..5000];; //false

//getClosestPair
//Larson
getClosestPair [(1.0, 1.0); (2.0, 2.0); (0.5, 0.5); (4.0, 4.0)];; //(0.5, 0.5)
getClosestPair [(1.0, 30.0); (2.0, 20.0); (3.0, 3.0)];; //(3.0, 3.0)
getClosestPair [(-1.0, 0.0); (1.0, 0.0); (0.0, 0.0)];; //(0.0, 0.0)
getClosestPair [];; //(infinity, infinity)
//Mine
getClosestPair [(1.0, 1.0); (2.0, 2.0); (0.5, 0.5); (4.0, 4.0); (-0.5, -0.5)];; //(0.5, 0.5)
getClosestPair [(1.0, 1.0); (2.0, 2.0); (0.5, 0.5); (4.0, 4.0); (-0.5, -0.5); (1.0, 1.0); (2.0, 2.0); (0.5, 0.5); (4.0, 4.0); (-0.5, -0.5); (1.0, 1.0); (2.0, 2.0); (0.5, 0.5); (4.0, 4.0); (-0.5, -0.5); (1.0, 1.0); (2.0, 2.0); (0.5, 0.5); (4.0, 4.0); (-0.5, -0.5); (0.0, 0.0)];; //(0.0, 0.0)
getClosestPair [(1.0, 1.0); (2.0, 2.0); (0.5, 0.5); (4.0, 4.0); (-0.5, -0.5); (-0.2, -0.2)];; //(-0.2, -0.2)
>>>>>>> @{-1}
