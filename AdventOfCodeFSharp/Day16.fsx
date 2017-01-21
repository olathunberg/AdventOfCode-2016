open System

let testInitialState = "10000"
let InitialState = "00111101111101000"

let inreverse (indata : string) =
    indata
    |> Seq.map (fun x ->
           match x with
           | '0' -> '1'
           | '1' -> '0'
           | _ -> ' ')
    |> Seq.rev
    |> String.Concat

let rec generate (indata : string) (size : int) =
    let dragon (indata : string) =
        [ indata
          "0"
          inreverse indata ]
        |> String.Concat

    let result = dragon indata
    if result.Length <= size then generate result size
    else result.[0..size - 1]

let rec checksum (indata : string) =
    let checksumround (indata : string) =
        indata
        |> Seq.chunkBySize 2
        |> Seq.map (fun x ->
               if x.[0] = x.[1] then "1"
               else "0")
        |> String.Concat

    let result = checksumround indata
    if result.Length % 2 = 0 then checksum result
    else result

let test = checksum (generate testInitialState 20) = "01100"
let answer1 = checksum (generate InitialState 272) = "10011010010010010"
let answer2 = checksum (generate InitialState 35651584) = "10101011110100011"
