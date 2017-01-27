open System
open System.Collections.Generic
open System.IO

let dir = "C:/Users/olath/OneDrive/Dokument/AdventOfCodeFSharp/AdventOfCodeFSharp"
let input = File.ReadAllLines (dir + "/Day20Input.txt")

let (|Int|_|) x =
  match Int64.TryParse x with
  | true, n -> Some n
  | _       -> None

let pairs =
    input
    |> Seq.choose (fun pair ->
        match pair.Split '-' with
        | [|Int start; Int stop|] -> Some (start, stop)
        | _                       -> None)
    |> Seq.sortBy fst
    |> Seq.toList

let allowedIPs input =
  let rec loop lo hi input = seq {
    match input with
    | [] -> ()
    | (lo', hi')::tl ->
      if lo' > hi + 1L then
        yield! { hi+1L .. lo'-1L}

      yield! loop lo (max hi hi') tl
  }

  loop 0L 0L input

let answer1 = allowedIPs pairs |> Seq.head

let answer2 = allowedIPs pairs |> Seq.length
