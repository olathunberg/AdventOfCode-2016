open System
open System.Collections.Generic

//The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
//The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
//The third floor contains a thulium-compatible microchip.
//The fourth floor contains nothing relevant.
type Item =
    | Generator of string
    | Microchip of string

type State =
    { floor : int
      items : Map<Item, int> }

let input =
    [ Microchip "T", 3
      Microchip "R", 2
      Microchip "C", 2
      Microchip "S", 1
      Microchip "P", 1
      Generator "T", 2
      Generator "R", 2
      Generator "C", 2
      Generator "S", 1
      Generator "P", 1 ]
    |> Map.ofList

let testInput =
    [ Microchip "H", 1
      Microchip "L", 1
      Generator "H", 2
      Generator "L", 3 ]
    |> Map.ofList

let floorIsValid items =
    let hasOwnGenerator i = items |> Seq.contains (Generator i)

    let chipsHasOwnGenerator =
        Seq.forall (function
            | Microchip m -> hasOwnGenerator m
            | _ -> true)

    let noGenerators =
        Seq.forall (function
            | Microchip _ -> true
            | _ -> false)

    noGenerators items || chipsHasOwnGenerator items

let itemsOnFloor n state =
    state.items
    |> Map.toSeq
    |> Seq.filter (fun (item, floor) -> floor = n)
    |> Seq.map fst
    |> Seq.toArray

let isValid (state : State) = Seq.forall (fun n -> floorIsValid (itemsOnFloor n state))
let finished state = state.items |> Map.forall (fun i floor -> floor = 4)

let getNextStates st =
    seq {
        let onThisFloor = itemsOnFloor st.floor st

        let doNotFry =
            function
            | Generator g, Microchip m -> g = m
            | Microchip m, Generator g -> m = g
            | _ -> true

        let availCargoPairs =
            [ for i in 0..onThisFloor.Length - 1 do
                  for j in i + 1..onThisFloor.Length - 1 do
                      let a, b = onThisFloor.[i], onThisFloor.[j]
                      if doNotFry (a, b) then yield [ a; b ] ]

        let possibleMoves newFloor cargo =
            seq {
                for move in cargo do
                    let newItems =
                        move |> List.fold (fun (items : Map<Item, int>) move -> items.Add(move, newFloor)) st.items

                    let newState =
                        { floor = newFloor
                          items = newItems }
                    if isValid newState [ st.floor; newFloor ] then yield newState
            }

        if st.floor < 4 then
            yield! possibleMoves (st.floor + 1) availCargoPairs
            yield! possibleMoves (st.floor + 1) (onThisFloor |> Seq.map (fun a -> [ a ]))
        if st.floor > 1 then
            yield! possibleMoves (st.floor - 1) availCargoPairs
            yield! possibleMoves (st.floor - 1) (onThisFloor |> Seq.map (fun a -> [ a ]))
    }

let isUnseen (visits : HashSet<State>) state =
    if visits.Contains(state) then false
    else visits.Add(state)

let solve start =
    let visits = new HashSet<State>()
    let toVisit = new Queue<State * int>()
    toVisit.Enqueue(start, 0)
    seq {
        while toVisit.Count > 0 do
            let (state, steps) = toVisit.Dequeue()
            if finished state then yield steps
            else
                for move in getNextStates state do
                    if isUnseen visits move then toVisit.Enqueue(move, steps + 1)
    }

let tmr = new System.Diagnostics.Stopwatch()

tmr.Restart()
printfn "Solution:   %d" (solve { floor = 1
                                  items = testInput }
                          |> Seq.head) // 11
printfn "Elapsed ms: %d" tmr.ElapsedMilliseconds

solve { floor = 1
        items = input }
|> Seq.head // 37

let inputPart2 =
    [ Microchip "T", 3
      Microchip "R", 2
      Microchip "C", 2
      Microchip "S", 1
      Microchip "P", 1
      Generator "T", 2
      Generator "R", 2
      Generator "C", 2
      Generator "S", 1
      Generator "P", 1
      Generator "E", 1
      Microchip "E", 1
      Generator "D", 1
      Microchip "D", 1 ]
    |> Map.ofList

solve { floor = 1
        items = inputPart2 }
|> Seq.head // 37
