open System
open System.IO

let indata =
    File.ReadAllLines
        ("C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day09Input.txt")
let testdata1 = "ADVENT" // "ADVENT"
let testdata2 = "A(1x5)BC" // "ABBBBBC"
let testdata3 = "(3x3)XYZ" // "XYZXYZXYZ"
let testdata4 = "A(2x2)BCD(2x2)EFG" // "ABCBCDEFEFG"
let testdata5 = "(6x1)(1x3)A" // "(1x3)A"
let testdata6 = "X(8x2)(3x3)ABCY" // "X(3x3)ABC(3x3)ABCY"

let (|Int|_|) str =
    match System.Int32.TryParse(str) with
    | (true, int) -> Some(int)
    | _ -> None

let isNumeric str =
    match str with
    | Int i -> i
    | _ -> -1

let isMarker (str : string) =
    let split = str.Split 'x'
    split.Length > 1 && (isNumeric (split.[0].TrimStart '(') >= 0) && (isNumeric (split.[1].TrimEnd ')') >= 0)

let getMarker (str : string) =
    let split = ((str.TrimStart '(').TrimEnd ')').Split 'x'
    ((Int32.Parse split.[0], Int32.Parse split.[1]), str.Length)

//let markerpositions (str : string) =
//    [ 0..str.LastIndexOf '(' ]
//    |> Seq.mapi (fun i input -> isMarker (str.Substring i).[..(str.Substring i).IndexOf(')')], i)
//    |> Seq.filter (fun x -> fst x)
//    |> Seq.map (fun x -> snd x)
let markerpositions (str : string) =
    let result = System.Collections.Generic.List<int>()
    let mutable i = 0
    let length =  str.LastIndexOf '('
    while i <= length do
        if isMarker (str.Substring i).[..(str.Substring i).IndexOf(')')] then
            result.Add i
            i <- i + 3
        else
            i <- i + 1
    result

let expandStr (str : string) (n : int) =
    [ 0..n - 1 ]
    |> Seq.map (fun c -> str)
    |> Seq.reduce (+)

let decompress (str : string) =
    let mutable i = 0
    let mutable j = 0
    let positions = markerpositions str
    while i < str.Length do
        if positions |> Seq.contains i then
            let marker = getMarker (str.Substring i).[..(str.Substring i).IndexOf(')')]
            let after = i + (snd marker)
            let len = snd (fst marker)
            let expanded = expandStr ((str.Substring after).[..fst (fst marker) - 1]) (len)
            //printf "%s" expanded
            j <- j + expanded.Length
            i <- i + (snd marker) + (fst (fst marker))
        else
            //printf "%c" str.[i]
            j <- j + 1
            i <- i + 1
    j

let answer1 = decompress (indata |> Seq.head)

// --- Part 2 ---
let rec decompress2 (str : string) =
    let mutable i = 0
    let mutable j = 0
    let markers = markerpositions str
    if markers.Count > 0 then
        while i < str.Length do
            if markers |> Seq.contains i then
                let subStr = str.Substring i
                let marker = getMarker subStr.[..subStr.IndexOf(')')]
                let after = i + (snd marker)
                let len = snd (fst marker)
                let expStr = expandStr ((str.Substring after).[..fst (fst marker) - 1]) (len)
                let expanded = decompress2 expStr
                i <- i + (snd marker) + (fst (fst marker))
                j <- j + expanded
            else
                j <- j + 1
                i <- i + 1
    else
        j <- str.Length
    j

// Denna går inte, tar för lång tid
let answer2 = decompress2 (indata |> Seq.head)

// Inspiration av theburningmunk
// Att split går att nyttja
// "ADVENT".Split([| '('; ')' |], 3) -> "ADVENT"
// "X(8x2)(3x3)ABCY".Split([| '('; ')' |], 3) -> "X", "3x3", "ABCY"
let rec decompressV2 input =
    let rec loop (input : string) acc =
        let mutable i = 0L
        let split = input.Split([| '('; ')' |], 3)
        if split.Length = 1 then
            i <- acc + int64 split.[0].Length
        else
            let subSplit = split.[1].Split 'x'
            let subN = isNumeric subSplit.[0]
            let subCount = isNumeric subSplit.[1]

            let repeatLen = split.[2].Substring(0, subN) |> decompressV2
            let acc = acc + int64 split.[0].Length + int64 subCount * repeatLen
            i <- loop (split.[2].Substring subN) acc
        i

    loop input 0L

let answerV2 = decompressV2 (indata |> Seq.head)


decompressV2 "X(8x2)(3x3)ABCY" // 20
decompressV2 "(27x12)(20x12)(13x14)(7x10)(1x12)A" // 241920
decompressV2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" // 445
