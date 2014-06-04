
// ----- Types -------------------
type Direction = | Up | Down | Left | Right

// 4x4
type Board = int [,]


// ----- Board utility functions ------

let transpose board =
    Array2D.init (board |> Array2D.length2) (board |> Array2D.length1) (fun i j -> board.[j,i])

let createSquare (rnd:System.Random) =
    if rnd.NextDouble() < 0.9 then 2 else 4

let pickRandom (rnd:System.Random) lst =
    let count = lst |> List.length
    let i =
        (rnd.NextDouble()) * (double count)
        |> floor
        |> int
    lst.[i]

let fillSquare rnd (grid:Board) =
    let i, j =
        [ for i in 0..3 do
            for j in 0..3 do
                if grid.[i,j] = 0 then yield (i,j) ]
        |> pickRandom rnd
    grid.[i,j] <- createSquare rnd
    grid


// ----- Move Functions --------

// [| 0; 2; 2; 4 |] -> [| 0; 0; 4; 4 |]
let pushRight row =
    let noZeros =
        row
        |> Array.filter ((<>) 0)

    Array.foldBack (fun i acc ->
        // acc -> (bool, int list) where true means it can mix
        match acc with
        | (_, []) -> (true, [i])
        | (true, hd::tl) -> if i = hd then (false, (2*i)::tl) else (true,i::hd::tl)
        | (false, lst) -> (true, i::lst)) noZeros (true, [])
    |> fun (b, lst) -> (lst |> List.toArray) |> Array.append (Array.zeroCreate (4-lst.Length))

// [| 0; 2; 2; 4 |] -> [| 4; 4; 0; 0 |]
let pushLeft row =
    let noZeros =
        row
        |> Array.filter ((<>) 0)

    Array.fold (fun acc i->
        // acc -> (bool, int list) where true means it can mix
        match acc with
        | (_, []) -> (true, [i])
        | (true, hd::tl) -> if i = hd then (false, (2*i)::tl) else (true,i::hd::tl)
        | (false, lst) -> (true, i::lst)) (true, []) noZeros
    |> fun (b, lst) -> (Array.zeroCreate (4-lst.Length)) |> Array.append (lst |> List.rev |> List.toArray)

let move rnd dir (board:Board) =
    let after =
        match dir with
        | Left -> seq { for i in 0..3 -> board.[i,*] |> pushLeft} |> array2D
        | Right -> seq { for i in 0..3 -> board.[i,*] |> pushRight} |> array2D
        | Up -> seq { for i in 0..3 -> board.[*,i] |> pushLeft } |> array2D |> transpose
        | Down -> seq { for i in 0..3 -> board.[*,i] |> pushRight } |> array2D |> transpose
    if after = board then None else Some (fillSquare rnd after)


// ------ Testing -------------

let rnd = new System.Random()

let board =
    Array2D.create 4 4 0
    |> fillSquare rnd
    |> fillSquare rnd

board
|> move rnd Up
|> move rnd Down
|> move rnd Right
|> move rnd Up

