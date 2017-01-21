open System
open System.IO

type Direction =
    | Up
    | Down
    | Left
    | Right

let DirectionFromChar x =
    match x with
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | _   -> Up

let indata sequence =
    sequence
    |> Seq.map (fun p -> DirectionFromChar (p))

let indataString =
    File.ReadAllLines
        ("C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day02Input.txt")

let move current step =
    match step with
    | Up    -> if [| 4; 5; 6; 7; 8; 9 |] |> Array.contains current then current-3 else current
    | Down  -> if [| 1; 2; 3; 4; 5; 6 |] |> Array.contains current then current+3 else current
    | Left  -> if [| 2; 3; 5; 6; 8; 9 |] |> Array.contains current then current-1 else current
    | Right -> if [| 1; 2; 4; 5; 7; 8 |] |> Array.contains current then current+1 else current

let walk sequence startpos =
    Seq.fold (fun (pos) (step) -> move pos step) (startpos) sequence

let answerPart1 =
//    [|"ULL";"RRDDD";"LURDL";"UUUUD"|]
    indataString
    |> Seq.scan (fun (start) (c) -> walk (indata c) start) 5
    |> Seq.skip 1
    |> Seq.iter (fun c -> printfn "%A" c)

let moveUp current =
    match current with
    | 3 -> 1
    | 6 | 7 | 8 -> current - 4
    | 10 | 11 | 12 -> current - 4
    | 13 -> 11
    | _ -> current

let moveDown current =
    match current with
    | 1 -> 3
    | 2 | 3 | 4 -> current + 4
    | 6 | 7 | 8 -> current + 4
    | 11 -> 13
    | _ -> current

let moveLeft current =
    match current with
    | 3 | 4 | 6 | 7 | 8 | 9 | 11 | 12 -> current - 1
    | _ -> current

let moveRight current =
    match current with
    | 2 | 3 | 5| 6 | 7 | 8 | 10 | 11 -> current + 1
    | _ -> current

let move2 current step =
    match step with
    | Up    -> moveUp current
    | Down  -> moveDown current
    | Left  -> moveLeft current
    | Right -> moveRight current

let walk2 sequence startpos =
    Seq.fold (fun (pos) (step) -> move2 pos step) (startpos) sequence

let answerPart2 =
//    [|"ULL";"RRDDD";"LURDL";"UUUUD"|]
    indataString
    |> Seq.scan (fun (start) (c) -> walk2 (indata c) start) 5
    |> Seq.skip 1
    |> Seq.iter (fun c -> printfn "%A" c)
