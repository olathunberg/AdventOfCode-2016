open System
open System.Collections.Generic
open System.IO

let dir = "C:/Users/olath/OneDrive/Dokument/AdventOfCodeFSharp/AdventOfCodeFSharp"
let input = File.ReadAllLines (dir + "/Day24Input.txt")
let testInput =
    ["###########";
     "#0.1.....2#";
     "#.#######.#";
     "#4.......3#";
     "###########"]
     |> Seq.toArray

let maze (input:string[]) =
    let x = input.[0].Length
    let y = input.Length
    let array = Array2D.init x y (fun x y -> 0)

    [0..y-1]
    |> Seq.iter (fun y ->
        [0..x-1]
        |> Seq.iter (fun x ->
            if input.[y].[x] = '#' then
                Array2D.set array x y -1))

    array

let points (input : string[]) =
    let x = input.[0].Length
    let y = input.Length
    let result = new List<int * (int * int)>()

    [0..y-1]
    |> Seq.iter (fun y ->
        [0..x-1]
        |> Seq.iter (fun x ->
            if input.[y].[x] <> '#' && input.[y].[x] <> '.' then
                result.Add ((int)(input.[y].[x])-(int)'0', (x, y))))

    result |> Seq.sortBy (fun c -> fst c)

let plot =
    let grid = maze testInput
    [0..(grid.GetLength 1)-1]
    |> Seq.iter (fun y ->
        [0..(grid.GetLength 0)-1]
        |> Seq.iter (fun x ->
            if x = (grid.GetLength 0)-1 then
                printfn ""
            else
                printf "%2d" grid.[x,y]))
    points testInput
    |> Seq.iter (fun c -> printfn "%d (%3d %3d)" (fst c) (fst (snd c)) (snd (snd c)))

let stepsbetween (input : string[]) p1 p2 =
    let moves =
        [|[|-1; 0|]; [|0;-1|];[|0;1|];[|1;0|]|]

    let workArray = maze input
    let maxX = workArray.GetLength 0
    let maxY = workArray.GetLength 1

    let dict = new Dictionary<(int * int), int>()
    // Kolla mot en Dict of point and step
    // om mindre step spara och fortsätt annars stanna
    let isValid x y maxX maxY n =
        let okStep = workArray.[x,y] = 0
        let inside = x >= 0 && y >= 0 && x < maxX && y < maxY
        if dict.ContainsKey (x,y) && dict.[(x,y)] > n then
            dict.[(x,y)] <- n
            okStep
        else if dict.ContainsKey (x,y) then
            false
        else
            dict.[(x,y)] <- n
            inside && okStep

    let visits = new Queue<int[] * int>()
    visits.Enqueue ([|(fst p1);(snd p1)|],0)

    seq {
        while visits |> Seq.length > 0 do
            let step = visits.Dequeue()

            for move in moves do
                let newX = (fst step).[0] + move.[0]
                let newY = (fst step).[1] + move.[1]

                if isValid newX newY maxX maxY (snd step) then
                    if newX = (fst p2) && newY = (snd p2) then
                        yield (snd step)+1
                    else
                        visits.Enqueue (([|newX;newY|]), ((snd step)+1))
        }

let getPointCoord c (p: seq<int * (int * int)>) =
    let pasChar =
        p
        |> Seq.filter (fun x -> fst x = c)
        |> Seq.head

    snd pasChar

let rec permutations (A : 'a list) =
    if List.isEmpty A then [[]] else
    [
        for a in A do
        yield! A |> List.filter (fun x -> x <> a)
                 |> permutations
                 |> List.map (fun xs -> a::xs)
    ]

let permSteps input =
    let list =
        points input
        |> Seq.map (fun x -> fst x)
        |> Seq.toList

    permutations (list)
    |> Seq.filter (fun x -> x.[0] = 0)

let permSteps2 input =
    let list =
        points input
        |> Seq.map (fun x -> fst x)
        |> Seq.toList

    permutations (list)
    |> Seq.filter (fun x -> x.[0] = 0)
    |> Seq.map (fun c -> List.append c [0])

let solve indata permsteps =
    let instr = indata
    let po = points instr
    let pp =
        po
        |> Seq.map (fun x -> snd x) |> Seq.toList

    let stepsBetweenPoints =
        seq {
            for i in 0..(pp |> Seq.length)-1 do
                for j in 0 .. (pp |> Seq.length)-1 do
                    if i <> j then
                        yield (pp.[i], pp.[j]), (stepsbetween instr pp.[i] pp.[j]) |> Seq.head }
        |> Seq.toList

    let shortestRoute p1 p2 =
        stepsBetweenPoints
        |> Seq.filter (fun m -> (fst (fst m) = p1 && snd (fst m) = p2) || (fst (fst m) = p2 && snd (fst m) = p1))
        |> Seq.head
        |> snd

    permsteps indata
    |> Seq.map(fun c ->
        c
        |> Seq.windowed 2
        |> Seq.sumBy (fun v -> let p1 = getPointCoord v.[0] po
                               let p2 = getPointCoord v.[1] po
                               shortestRoute p1 p2))

    |> Seq.sortBy (fun x -> x)
    |> Seq.head

let solve1 indata =
    solve indata permSteps

solve1 testInput // 14

let tmr = new System.Diagnostics.Stopwatch()
tmr.Restart()
printfn "Solution:   %d" (solve1 input) // 462
printfn "Elapsed ms: %d" tmr.ElapsedMilliseconds


let solve2 indata =
    solve indata permSteps2

solve2 input // 676
