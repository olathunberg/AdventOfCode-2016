open System
open System.Security.Cryptography

let MD5Hash (input : string) =
     use md5 = System.Security.Cryptography.MD5.Create()
     input
     |> System.Text.Encoding.ASCII.GetBytes
     |> md5.ComputeHash
     |> Seq.map (fun c -> c.ToString("X2"))
     |> Seq.reduce (+)

let isInteresting input =
    let hash = MD5Hash input
    if hash.Substring(0, 5) = "00000" then
        (true, hash.[5])
    else
        (false, 'x')

let calc indata =
    let mutable i = 0
    let mutable index = 0
    let result = Array.create 8 ' '
    while index < 8 do
        let data = String.Format("{0}{1}", indata, i)
        let hash = isInteresting data
        if fst hash then
            printfn "%s" data
            Array.set result index (snd hash)
            index <- index + 1
        i <- i+1
    result

calc "cxdnnyjw"

let isInteresting2 input =
    let hash = MD5Hash input
    if hash.Substring(0, 5) = "00000" then
        if int (hash.[5]) >= int '0' && int (hash.[5]) <= int '9' then
            (true, (Int32.Parse (hash.[5].ToString()), hash.[6]))
        else
            (false, (0, 'x'))
    else
        (false, (0, 'x'))

let calc2 indata =
    let mutable i = 0
    let mutable index = 0
    let result = Array.create 8 ' '
    while index < 8 do
        let data = String.Format("{0}{1}", indata, i)
        let hash = isInteresting2 data
        if fst hash && fst (snd hash) < 8 then
            let pos = fst (snd hash)
            let value = (snd (snd hash))
            if Array.get result pos = ' ' then
                Array.set result pos value
                index <- index + 1
                printfn "%s" (result |> Seq.map (fun c -> c.ToString().ToLower()) |> Seq.reduce (+))
        i <- i+1
    result

//calc2 "abc"
calc2 "cxdnnyjw"
