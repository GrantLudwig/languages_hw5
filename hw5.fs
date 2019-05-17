let rec sumEven list =
    match list with
    |[] -> 0
    |hd::tl -> 
        if hd % 2 = 0 then hd + sumEven tl else sumEven tl