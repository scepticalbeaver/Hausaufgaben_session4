let rec map f list =
    match list with
    | head :: tail -> f head :: map f tail
    | [] -> []

let insertTrd x list =
    match list with
    | a :: b :: tail -> a :: b :: x :: tail
    | a :: [] -> a :: x :: []
    | [] -> [x]


printfn "%A" <| insertTrd 7 [1..5]