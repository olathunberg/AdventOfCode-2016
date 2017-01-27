open System
open System.IO

let indata =
    File.ReadAllLines
        ("C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day07Input.txt")

// "C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day07TestInput.txt")
let isAbba (indata : string []) =
    indata |> Seq.mapi (fun i input ->
                  [ 0..((Seq.length input) - 4) ]
                  |> Seq.mapi
                         (fun item x ->
                         (input.[item] = input.[item + 3]) && (input.[item + 1] = input.[item + 2])
                         && (input.[item] <> input.[item + 1]))
                  |> Seq.exists (fun c -> c = true), i % 2 = 0)

let abbas (indata : string []) =
    indata
    |> Seq.map (fun c -> c.Split([| '['; ']' |]))
    |> Seq.map (fun c -> isAbba c)
    |> Seq.map
           (fun c ->
           (c |> Seq.exists (fun v -> fst v && snd v)) && not (c |> Seq.exists (fun v -> fst v && not (snd v))))
    |> Seq.filter (fun c -> c = true)
    |> Seq.length

let answer1 = abbas indata

let getAbas (indata : string) =
    indata.Split([| '['; ']' |])
    |> Seq.mapi (fun i input ->
           [ 0..((Seq.length input) - 3) ]
           |> Seq.filter (fun item -> (input.[item] = input.[item + 2]) && (input.[item] <> input.[item + 1]))
           |> Seq.map (fun item -> (sprintf "%c%c%c" input.[item] input.[item + 1] input.[item + 2])), i)

let isOpposite (x : string) (y : string) = x.[0] = y.[1] && x.[2] = y.[1] && x.[1] = y.[0] && x.[1] = y.[2]

let getSupernets (a : seq<seq<string> * int>) =
    a
    |> Seq.filter (fun x -> snd x % 2 = 0)
    |> Seq.map (fun x -> fst x)
    |> Seq.concat

let getHypernets (a : seq<seq<string> * int>) =
    a
    |> Seq.filter (fun x -> snd x % 2 = 1)
    |> Seq.map (fun x -> fst x)
    |> Seq.concat

let isSSL c =
    let abas = getAbas c
    let supernets = getSupernets abas
    let hypernets = getHypernets abas
    supernets
    |> Seq.filter (fun c -> (hypernets |> Seq.exists (fun v -> isOpposite v c)))
    |> Seq.length
    > 0

let answer2 =
    indata
    |> Seq.filter (fun c -> isSSL c)
    |> Seq.length
