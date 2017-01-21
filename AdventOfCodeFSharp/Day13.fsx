open System
open System.Collections.Generic

let isFree x y n =
    let init = x*x + 3*x + 2*x*y + y + y*y + n
    let binString = Convert.ToString (init, 2)

    let noOnes =
        binString
        |> Seq.filter (fun x -> x = '1')
        |> Seq.length

    if noOnes % 2 = 0 then
        true
    else
        false

let getGrid rows cols init =
    let result = Array2D.init cols rows (fun x y -> ".")
    [0..rows-1]
    |> Seq.iter (fun y ->
        [0..cols-1]
        |> Seq.iter (fun x ->
            let free = isFree x y init
            if free then
                Array2D.set result x y "."
            else
                Array2D.set result x y "#"))

    Array2D.set result 1 1 "1"
    Array2D.set result 31 39 "2"

    result

//let grid = getGrid 70 90 1362
let grid = getGrid 70 90 1362

// Plot grid
[0..(grid.GetLength 1)-1]
|> Seq.iter (fun y ->
    [0..(grid.GetLength 0)-1]
    |> Seq.iter (fun x ->
        if x = (grid.GetLength 0)-1 then
            printfn ""
        else
            printf "%s" grid.[x,y]))

let intGrid rows cols init =
    let result = Array2D.init cols rows (fun x y -> 0)
    [0..rows-1]
    |> Seq.iter (fun y ->
        [0..cols-1]
        |> Seq.iter (fun x ->
            let free = isFree x y init
            if free then
                Array2D.set result x y 0
            else
                Array2D.set result x y -1))

    Array2D.set result 1 1 1
    result

let solve (targetX : int) (targetY : int) (giveup : int) =
    let moves =
        [|[|-1; 0|]; [|0;-1|];[|0;1|];[|1;0|]|]

    let isValid x y maxX maxY=
        x >= 0 && y >= 0 && x < maxX && y < maxY
    let workGrid = (intGrid 100 100 1362)
    let maxX = workGrid.GetLength 0
    let maxY = workGrid.GetLength 1

    let rec doMove indata x y (count:int) =
        let array = Array2D.copy indata
        let result = new List<int>()

        if array.[x,y] >= 1 && count <= giveup then
            for move in moves do
                let newX = x + move.[0]
                let newY = y + move.[1]

                if isValid newX newY maxX maxY then
                    if array.[newX,newY] = 0 then
                        array.[newX,newY] <- array.[newX,newY] + 1
                        printfn "%02d-%02d %d" newX newY (count+1)
                        let tmp = doMove array newX newY (count+1)
                        result.Add tmp
                    else if array.[newX,newY] = -99 then
                        result.Add (count+1)

        if result |> Seq.length = 0 then
            result.Add giveup
        result  |> Seq.sort |> Seq.head

    if workGrid.[targetX, targetY] = -1 then
        giveup
    else
        Array2D.set workGrid targetX targetY -99
        doMove workGrid 1 1 0

let part1 = solve 31 39 1000000


let solve2 (giveup : int) =
    let moves =
        [|[|-1; 0|]; [|0;-1|];[|0;1|];[|1;0|]|]

    let isValid x y maxX maxY=
        x >= 0 && y >= 0 && x < maxX && y < maxY
    let workGrid = (intGrid 100 100 1362)
    let maxX = workGrid.GetLength 0
    let maxY = workGrid.GetLength 1

    let rec doMove indata x y (count:int) =
        let array = Array2D.copy indata
        let result = new List<int * (int * int)>()

        if array.[x,y] >= 1 && count <= giveup then
            for move in moves do
                let newX = x + move.[0]
                let newY = y + move.[1]

                if isValid newX newY maxX maxY then
                    if array.[newX,newY] = 0 then
                        array.[newX,newY] <- array.[newX,newY] + 1
                        let n = count + 1
//                        printfn "%02d-%02d %d" newX newY n
                        result.Add (n, (newX, newY))
                        result.AddRange (doMove array newX newY (count+1))

        result

    doMove workGrid 1 1 0

// Part 1
solve2 100
|> Seq.filter (fun c -> snd c = (31,39))
|> Seq.sort
|> Seq.head

// Part 2
solve2 100
|> Seq.filter (fun c -> (fst c) <= 50)
|> Seq.distinctBy (fun c -> snd c)
|> Seq.length
|> (+) 1
