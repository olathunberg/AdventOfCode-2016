open System
open System.IO

let indata =
    File.ReadAllLines
        ("C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day06Input.txt")


let rotate (indata : string []) =
    seq {
        for i in 0..(indata |> Seq.head |> String.length) - 1 do
            yield indata
                  |> Seq.map (fun c -> c.[i])
                  |> Seq.groupBy (fun c -> c)
                  |> Seq.sortByDescending (fun c -> snd c |> Seq.length)
                  |> Seq.map (fun (key, value) -> key)
                  |> Seq.head
    }

rotate indata
    |> Seq.map (fun c -> c.ToString())
    |> Seq.reduce (+)

let rotate2 (indata : string []) =
    seq {
        for i in 0..(indata |> Seq.head |> String.length) - 1 do
            yield indata
                  |> Seq.map (fun c -> c.[i])
                  |> Seq.groupBy (fun c -> c)
                  |> Seq.sortBy (fun c -> snd c |> Seq.length)
                  |> Seq.map (fun (key, value) -> key)
                  |> Seq.head
    }

rotate2 indata
    |> Seq.map (fun c -> c.ToString())
    |> Seq.reduce (+)
