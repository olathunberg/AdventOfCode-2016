open System
open System.IO

let parseToInt sequence =
    sequence
    |> Seq.map (fun c -> System.Int32.Parse(c))
    |> Seq.toList

let isTrianglePossible (intTriangle: list<int>) =
    if Seq.length intTriangle = 3 then
        (intTriangle.[0] < intTriangle.[1] + intTriangle.[2])
        && (intTriangle.[1] < intTriangle.[0] + intTriangle.[2])
        && (intTriangle.[2] < intTriangle.[0] + intTriangle.[1])
    else false

let indata =
    File.ReadAllLines
        ("C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day03Input.txt")
    |> Seq.map (fun c -> c.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map parseToInt

let rotateSeq (input: seq<int List>) =
    let step = Seq.length (input |> Seq.item 0)
    let outer = Array.create (Seq.length input) Seq.empty<int>
    for i in 0 .. step .. (Seq.length input)-1 do
        for j in 0 .. step-1  do
            let sub = Array.create step 0
            for k in 0 .. step-1 do
                sub.[k] <- (input |> Seq.item (i+k) |> Seq.item j)
            outer.[i+j] <- Seq.ofArray sub
    Seq.ofArray outer

let answer1 =
    indata
    |> Seq.where (fun c -> isTrianglePossible (c))
    |> Seq.length

let answer2 =
    rotateSeq indata
    |> Seq.where (fun c -> isTrianglePossible (Seq.toList c))
    |> Seq.length