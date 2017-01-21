open System
open System.IO

type room =
    { description : string
      fullDescription : string
      checksum : string
      sectorId : int }

let implode (xs : string list) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    sb.ToString()

let getSectorId (input : string) = System.Int32.Parse(input.Split([| '[' |]).[0])
let getChecksum (input : string) = input.Split([| '[' |]).[1].TrimEnd(']')
let getDescription (input : string) = implode (input.Split([| '-' |]) |> Seq.toList)

let parseRoom (input : string) =
    let split = input.Split([| '-' |], StringSplitOptions.RemoveEmptyEntries)
    let sectorId = getSectorId (Seq.last split)
    let checkSum = getChecksum (Seq.last split)
    let fullDescription = input.Substring(0, (input.LastIndexOf '-'))
    let description = getDescription fullDescription
    { fullDescription = fullDescription
      description = description
      sectorId = sectorId
      checksum = checkSum }

//let theroom = parseRoom "aaaaa-bbb-z-y-x-123[abxyz]"
//let theroom = parseRoom "a-b-c-d-e-f-g-h-987[abcde]"
//let theroom = parseRoom "not-a-real-room-404[oarel]"
//let theroom = parseRoom "totally-real-room-200[decoy]"
let calcCheckSum (input : string) =
    input.ToCharArray()
    |> Seq.groupBy (fun c -> c)
    |> Seq.map (fun (c, grp) -> (c, grp |> Seq.length))
    |> Seq.sortBy (fun (c, ln) -> (-ln, c))
    |> Seq.take 5
    |> Seq.map (fun c -> fst c)
    |> Seq.toArray

let isRoomReal room = calcCheckSum room.description = room.checksum.ToCharArray()
let indata =
    File.ReadAllLines
        ("C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day04Input.txt")

let answer1 =
    indata
    |> Seq.map (fun c -> parseRoom c)
    |> Seq.filter (fun c -> isRoomReal c)
    |> Seq.sumBy (fun c -> c.sectorId)

let rotate (input : string) cycles =
    let realCycles = cycles % 26
    input.ToCharArray()
    |> Seq.map (fun c ->
           if c = '-' then 0
           else (int) c + realCycles)
    |> Seq.map (fun c ->
           if c > (int) 'z' then c - (int) 'z' + (int) 'a' - 1
           else c)
    |> Seq.map (fun c ->
           if c = 0 then ' '
           else char (c))
    |> String.Concat

let test = "qzmt-zixmtkozy-ivhz"

rotate test 343

let answer2 =
    indata
    |> Seq.map (fun c -> parseRoom c)
    |> Seq.filter (fun c -> isRoomReal c)
    |> Seq.map (fun c -> (rotate c.fullDescription c.sectorId, c.sectorId))
    |> Seq.filter (fun c -> (fst c).Contains "north")
    |> Seq.toList
