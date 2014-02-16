let rec map f list =
    match list with
    | head :: tail -> f head :: map f tail
    | [] -> []

let insertTrd x list =
    match list with
    | a :: b :: tail -> a :: b :: x :: tail
    | a :: [] -> a :: x :: []
    | [] -> [x]


//printfn "%A" <| insertTrd 7 [1..5]

let reverse list =
    let rec revAux list buff =
        match list with
        | head :: tail -> revAux tail (head :: buff)
        | [] -> buff
    revAux list []

//printfn "%A" <| reverse [1..5]

let rec concat list1 list2 =
    match list1 with
    | head :: tail ->  head :: (concat tail list2)
    | [] -> list2


printfn "%A" <| concat [1..5] [6..10]