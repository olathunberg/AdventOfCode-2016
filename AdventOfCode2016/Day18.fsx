open System

let inputData = "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."
let testData = ".^^.^.^^^^"

let traps (input : string) =
    input |> Seq.map (fun x ->
                 match x with
                 | '^' -> true
                 | _ -> false)

let isTrap (input : seq<bool>) (idx : int) =
    let indata = input |> Seq.toList
    (indata.[idx - 1] && indata.[idx] && not indata.[idx + 1])
    || (not indata.[idx - 1] && indata.[idx] && indata.[idx + 1])
    || (indata.[idx - 1] && not indata.[idx] && not indata.[idx + 1])
    || (not indata.[idx - 1] && not indata.[idx] && indata.[idx + 1])

let getNext (input : seq<bool>) =
    let prevRow =
        seq {
            yield false
            yield! input |> Seq.toArray
            yield false
        }
        |> Seq.toList
    prevRow
    |> Seq.skip 1
    |> Seq.take ((prevRow |> Seq.length) - 2)
    |> Seq.mapi (fun x i -> isTrap prevRow (x + 1))

let getRows indata =
    seq {
        yield indata
        yield! indata |> Seq.unfold (fun row ->
            let next = getNext row
            Some(next, next))
    }

let solve input n =
    getRows input
    |> Seq.take n
    |> Seq.toList
    |> Seq.sumBy (fun row ->
           row
           |> Seq.filter (fun c -> c = false)
           |> Seq.length)

getRows (traps testData) |> Seq.take 2
solve (traps testData) 10
solve (traps inputData) 40
solve (traps inputData) 400000
