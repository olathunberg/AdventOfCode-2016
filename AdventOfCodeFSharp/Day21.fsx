open System
open System.IO
open System.Text.RegularExpressions

let swap (str : string) x y =
    let arr = str |> Seq.toArray
    arr.[x] <- str.[y]
    arr.[y] <- str.[x]
    arr |> System.String

let swapletter (str : string) x y =
    let idx1 = str |> Seq.findIndex (fun c -> c = x)
    let idx2 = str |> Seq.findIndex (fun c -> c = y)
    swap str idx1 idx2

let rotateLeft (str : string) x =
    let ls = str |> List.ofSeq
    List.fold (fun (s, c) e ->
        if s <> 0 then (s - 1, List.append c.Tail [ e ])
        else (0, c)) (x, ls) ls
    |> fun (x, y) ->
        y
        |> List.ofSeq
        |> Seq.toArray
        |> System.String

let rotateRight (str : string) n =
    let tail = str.Substring(str.Length-n, n)
    sprintf "%s%s" tail str.[0..str.Length-(n+1)]

let rec rotate (str:string) x rev =
    if rev then
        let mutable temp = rotateLeft str 1
        while rotate temp x false <> str do
            temp <- rotateLeft temp 1
        temp
    else
        let idx = str |> Seq.findIndex (fun c -> c = x)
        let temp = rotateRight str idx

        let rotates = if idx >= 4 then 2 else 1
        rotateRight temp rotates

let move (str : string) x y =
    let temp = str.Substring(x, 1)
    let removed = str.Remove(x, 1)
    removed.Insert(y, temp)

let reverse (str : string) x y =
    let rev =
        str.Substring(x, (y+1) - x)
        |> Seq.rev
        |> Seq.toArray
        |> System.String
    sprintf "%s%s%s" (str.Substring(0, x)) (rev) (str.Substring(y+1))

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int|) x  = Int32.Parse x
let (|Char|) x  = Char.Parse x

let dir = "C:/Users/olath/OneDrive/Dokument/AdventOfCodeFSharp/AdventOfCodeFSharp"
let input = File.ReadAllLines (dir + "/Day21Input.txt")
let puzzleInput = "abcdefgh"

let solve init indata rev =
    let mutable str = init
    indata
    |> Array.iter (fun line ->
    match line with
    | Regex "rotate left (\d*) step" [Int steps] -> str <- if rev then rotateRight str steps else rotateLeft str steps
    | Regex "rotate right (\d*) step" [Int steps] -> str <- if rev then rotateLeft str steps else rotateRight str steps
    | Regex "rotate left (\d*) steps" [Int steps] -> str <- if rev then rotateRight str steps else rotateLeft str steps
    | Regex "rotate right (\d*) steps" [Int steps] -> str <- if rev then rotateLeft str steps else rotateRight str steps
    | Regex "swap letter (\w*) with letter (\w*)" [Char x; Char y] -> str <- swapletter str x y
    | Regex "swap position (\d*) with position (\d*)" [Int x; Int y] -> str <- swap str x y
    | Regex "move position (\d*) to position (\d*)" [Int x; Int y] -> str <- if rev then move str y x else  move str x y
    | Regex "rotate based on position of letter (\w*)" [Char st] -> str <- rotate str st rev
    | Regex "reverse positions (\d*) through (\d*)" [Int x; Int y] -> str <- reverse str x y
    | _ -> str <- "")

    str

solve puzzleInput input false

let puzzle2Input = "fbgdceah"
let input2 = input |> Seq.rev |> Seq.toArray
solve puzzle2Input input2 true

rotate "decab" 'd' true

rotate "ecabd" 'd' false

move "bdeac" 4 1
