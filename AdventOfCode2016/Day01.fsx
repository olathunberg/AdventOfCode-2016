open System
open System.IO

type Direction =
    | North
    | East
    | South
    | West

type Turn =
    | R
    | L

let turnFromChar x =
    match x with
    | 'R' -> R
    | 'L' -> L
    | _ -> L

let indata (a : string) =
    a.Split ','
    |> Seq.map (fun p -> p.Replace(" ", ""))
    |> Seq.map (fun p -> (turnFromChar (p.[0]), System.Int32.Parse(p.Substring(1, p.Length - 1))))

let leftTurn dir =
    match dir with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let rightTurn dir =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let doTurn current move =
    match move with
    | R -> rightTurn current
    | L -> leftTurn current

let updatePosition (currentDirection, (currentPositionX, currentPositionY)) (turn, steps) =
    let newDirection = doTurn currentDirection turn

    [| 1 .. steps |]
        |> Seq.scan (fun (dir, (x, y)) step ->
        match newDirection with
        | North -> (newDirection, (x + 1, y))
        | East -> (newDirection, (x, y + 1))
        | South -> (newDirection, (x - 1, y))
        | West -> (newDirection, (x, y - 1))) (currentDirection, (currentPositionX, currentPositionY))
        |> Seq.skip 1

let indataString =
    File.ReadAllText
        ("C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day01Input.txt")

let walk sequence =
    Seq.fold (fun (dir, (x, y)) (turn, steps) -> updatePosition (dir, (x, y)) (turn, steps) |> Seq.last ) (North, (0, 0)) sequence

let answerPart1 =
    let endPosition = walk (indata indataString)
    Math.Abs(fst (snd endPosition)) + Math.Abs(snd (snd endPosition))

let aggregatePositions sequence =
    let mutable Direction = North
    let mutable Position = (0, 0)

    seq { for item in sequence do
            yield! updatePosition (Direction, Position) (fst item, snd item)
            let tre =
                updatePosition (Direction, Position) (fst item, snd item) |> Seq.last
            Direction <- fst tre
            Position <- snd tre
        }

let answerPart2 =
    let positions =
//        aggregatePositions (indata "R8, R4, R4, R8")
        aggregatePositions (indata indataString)
    let endPosition =
        positions
        |> Seq.groupBy (fun v -> snd v)
        |> Seq.find (fun c -> snd c |> Seq.length > 1)
    Math.Abs(fst (fst endPosition)) + Math.Abs(snd (fst endPosition))
