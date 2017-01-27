open System
open System.Collections.Generic
open System.Security.Cryptography

let input = "vkjiggvb"
let testInput1 = "ihgpwlah" // DDRRRD
let testInput2 = "kglvqrro" // DDUDRLRRUDRD
let testInput3 = "ulqzkmiv" // DRURDRUDDLLDLUURRDULRLDUUDDDRR

let MD5Hash (input : string) =
     use md5 = System.Security.Cryptography.MD5.Create()
     input
     |> System.Text.Encoding.ASCII.GetBytes
     |> md5.ComputeHash
     |> Seq.map (fun c -> c.ToString("x2"))
     |> Seq.take 4
     |> Seq.reduce (+)

let moves =
    [|([|-1; 0|]), "L";
      ([| 0;-1|]), "U";
      ([| 0; 1|]), "D";
      ([| 1; 0|]), "R"|]

let isValid x y (hash:string) direction =
    let inside = x >= 0 && y >= 0 && x < 4 && y < 4
    let isOpen =
        match direction with
        | 'U' -> hash.[0] > 'a'
        | 'D' -> hash.[1] > 'a'
        | 'L' -> hash.[2] > 'a'
        | 'R' -> hash.[3] > 'a'
        | _ -> false
    inside && isOpen

let run (input : string) =
    let visits = new List<int[] * string>()
    visits.Add ([|0;0|],"")

    seq {
        while visits |> Seq.length > 0 do
            let steps = visits |> Seq.head
            let hash = MD5Hash (input+(snd steps))

            for move in moves do
                let newX = (fst steps).[0] + (fst move).[0]
                let newY = (fst steps).[1] + (fst move).[1]

                if isValid newX newY hash (Convert.ToChar(snd move)) then
                    if newX = 3 && newY = 3 then
                        yield (([|newX;newY|]), ((snd steps)+(snd move)))
                    else
                        visits.Add (([|newX;newY|]), ((snd steps)+(snd move)))

            visits.Remove steps |> ignore
        }

let part1 = // RDRRULDDDR
    run input
    |> Seq.head
    |> snd

let part2 = // 392
    run input
    |> Seq.last
    |> snd
    |> Seq.length
