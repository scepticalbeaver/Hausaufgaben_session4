let rec factorial x =
    match x with
    | 0 -> 1
    | _ -> x * factorial (x - 1)

printfn "%A" <| factorial 4


let rec fibNum x =
    match x with
    | 1 | 2 -> 1
    | _ -> fibNum (x - 1) + fibNum (x - 2)

printfn "%A" <| fibNum 10