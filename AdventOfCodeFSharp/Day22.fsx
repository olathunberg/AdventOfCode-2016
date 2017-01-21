open System
open System.IO
open System.Collections.Generic

let dir = "C:/Users/olath/OneDrive/Dokument/AdventOfCodeFSharp/AdventOfCodeFSharp"
let input = File.ReadAllLines(dir + "/Day22Input.txt") |> Seq.skip 2
let Int(x : string) = Int32.Parse(x.TrimEnd('T'))

type Node =
    { X : int
      Y : int
      Index : int
      Size : int
      Used : int
      Avail : int }

let getNodes =
    let nodes = new List<Node>()
    input |> Seq.iteri (fun i c ->
                 let split = c.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                 let posSplit = split.[0].Split [|'-'|]

                 let node =
                     { X = Int32.Parse(posSplit.[1].TrimStart 'x')
                       Y = Int32.Parse(posSplit.[2].TrimStart 'y')
                       Index = i
                       Size = Int split.[1]
                       Used = Int split.[2]
                       Avail = Int split.[3] }
                 nodes.Add node)
    nodes

let getMatches nodes node =
    nodes
        |> Seq.filter (fun b -> b.Avail >= node.Size)
        |> Seq.map (fun c -> node, c)

let part1 =
    let nodes = getNodes

    nodes
        |> Seq.filter (fun x -> x.Used > 0)
        |> Seq.map (fun x -> getMatches nodes x)
        |> Seq.concat
        |> Seq.length


let sortedNodes =
    getNodes
        |> Seq.sortBy (fun c -> c.Y, c.X)

let maxYnode =
    sortedNodes
        |> Seq.maxBy (fun c -> c.Y)

// Plot grid
[0..maxYnode.Y]
    |> Seq.mapi (fun i c ->
        sortedNodes
        |> Seq.filter (fun x -> x.Y = i)
        |> Seq.map (fun x -> sprintf "%3d " x.Used)
        |> Seq.reduce (+))
    |> Seq.toList

let solve2 =
    let emptyNode =
        getNodes
            |> Seq.filter (fun c -> c.Used = 0)
            |> Seq.head

    let targetNode =
        getNodes
            |> Seq.filter (fun c -> c.Y = 0)
            |> Seq.sortBy (fun c -> c.X)
            |> Seq.last

    let emptyNodeToBeforeTarget = emptyNode.X + targetNode.X - 1 + emptyNode.Y

    // Takes 5 moves of empty data to step target one step to the left
    // exept the last step
    emptyNodeToBeforeTarget + 5*(targetNode.X-1) + 1