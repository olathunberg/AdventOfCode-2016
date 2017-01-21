open System
open System.IO
open System.Security.Cryptography
open System.Text

let MD5Hash (input : string) =
    use md5 = System.Security.Cryptography.MD5.Create()
    let bytes = input |> Encoding.UTF8.GetBytes |> md5.ComputeHash
    BitConverter.ToString(bytes).Replace("-", "").ToLower()

let input = "qzyelonm"

let hasTriplet (str:string) =
    let index =
        [0 .. str.Length-3]
        |> Seq.filter (fun i -> str.[i] = str.[i+1] && str.[i] = str.[i+2])
    match index |> Seq.tryHead with
        | None   -> (false,  (Char.Parse " "))
        | Some c -> (true, (char)str.[c])

let hasFifthlet (str:string) (index:int) (m: char) =
    let hash = MD5Hash (String.Format("{0}{1}", str, index))
    [0 .. hash.Length - 5]
        |> Seq.exists (fun i -> hash.[i] = m && hash.[i] = hash.[i+1] && hash.[i] = hash.[i+2] && hash.[i] = hash.[i+3] && hash.[i] = hash.[i+4])

let isKeyHash (indata: string) (index:int) =
    let mutable result = false
    let hash = MD5Hash (String.Format("{0}{1}", indata, index))
    let isTriplet = hasTriplet hash
    let mChar = snd isTriplet

    if (fst isTriplet) then
        [index + 1 .. index + 1000]
        |> Seq.exists (fun i -> hasFifthlet indata i mChar)
    else
        false

let getKeys indata =
    let mutable i = 0
    let mutable index = 0

    while index < 64 do
        let hash = isKeyHash indata i
        if hash then
            printfn "%d - %d" index i
            index <- index + 1
        i <- i+1

let answer1 = getKeys input // 15168

let MD2016Hash (input : string) =
    { 0..2016 } |> Seq.fold (fun last _ -> MD5Hash last) input

let hasFifthlet2016 (hashes: seq<int*string>) (index:int) (m: char) =
    let hash = snd (hashes |> Seq.item index)
    [0 .. hash.Length - 5]
        |> Seq.exists (fun i -> hash.[i] = m && hash.[i] = hash.[i+1] && hash.[i] = hash.[i+2] && hash.[i] = hash.[i+3] && hash.[i] = hash.[i+4])

let isKeyHash2016 (indata: string) (index:int) (hashes: seq<int*string>)=
    let mutable result = false
    let isTriplet = hasTriplet (snd (hashes |> Seq.item index))

    if (fst isTriplet) then
        let mChar = snd isTriplet
        [index + 1  .. index + 1000]
        |> Seq.exists (fun i -> hasFifthlet2016 hashes i mChar)
    else
        false

// Lite cachning.skulle göra susen, kollar om den har 5dubletter mest hela tiden..
let getKeys2016 indata =
    let mutable i = 0
    let mutable index = 0
    let hashes =
        Seq.initInfinite id
        |> Seq.map (fun idx -> idx, sprintf "%s%d" indata idx |> MD2016Hash)
        |> Seq.cache

    while index < 64 do
        let hash = isKeyHash2016 indata i hashes
        if hash then
            printfn "%d - %d" index i
            index <- index + 1
        i <- i + 1

let answer2 = getKeys2016 input // 20864