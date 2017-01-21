open System

let testInput = 5
let puzzleInput = 3018458

let rec getNext (e : System.Collections.Generic.Dictionary<int, int>) (idx : int) (n : int) =
    let i =
        if idx = n then 1
        else idx+1
    if e.ContainsKey(i) then i
    else getNext e (i) n

let solve n next =
    let elfs = new System.Collections.Generic.Dictionary<int, int>()
    [ 1..n ] |> Seq.iter (fun x -> elfs.[x] <- 1)
    let rec round n =
        let mutable finalElf = -1
        [ 1..n ] |> Seq.iter (fun idx ->
                        if elfs.ContainsKey idx then
                            let toTakeFrom = next elfs idx n
                            if toTakeFrom >= 1 && toTakeFrom <> idx then
                                elfs.[idx] <- elfs.[idx] + elfs.[toTakeFrom]
                                elfs.Remove toTakeFrom |> ignore
                                if elfs.[idx] = n then finalElf <- idx)
        if finalElf = -1 then round n
        else finalElf
    round n

solve testInput getNext // 3

let tmr = new System.Diagnostics.Stopwatch()
tmr.Restart()
printfn "Solution:   %d" (solve puzzleInput getNext) // 1842613
printfn "Elapsed ms: %d" tmr.ElapsedMilliseconds

let solve2 n =
    let rec recSolve2 (state : System.Collections.Generic.List<int>) len pos =
        if len = 1 then state.[0]
        else
            let elf = state.[pos]
            //if len % 1000 = 0 then printfn "%d" len
            let oppositeIndex = (pos + len / 2) % len
            state.RemoveAt oppositeIndex
            let leftPos =
                if oppositeIndex > pos then (pos + 1) % (len - 1)
                else pos % (len - 1)
            recSolve2 state (len - 1) leftPos

    let init = new System.Collections.Generic.List<int>()
    [ 1..n ] |> Seq.iter (fun n -> init.Add n |> ignore)
    recSolve2 init n 0

solve2 testInput |> printfn "%d" // 2
let tmr2 = new System.Diagnostics.Stopwatch()
tmr2.Restart()
printfn "Solution:   %d" (solve2 puzzleInput) // 1424135
printfn "Elapsed ms: %d" tmr2.ElapsedMilliseconds // 957710 (~16min)
