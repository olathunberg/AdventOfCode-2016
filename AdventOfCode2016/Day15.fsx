open System

type Disc =
    { Positions : int
      Init : int }

let testinput =
    [ { Positions = 5
        Init = 4 }
      { Positions = 2
        Init = 1 } ]

let input =
    [ { Positions = 7
        Init = 0 }
      { Positions = 13
        Init = 0 }
      { Positions = 3
        Init = 2 }
      { Positions = 5
        Init = 2 }
      { Positions = 17
        Init = 0 }
      { Positions = 19
        Init = 7 } ]

let isCollector (input: List<Disc>) (idx:int) =
    input
    |> Seq.indexed
    |> Seq.forall (fun x -> ((snd x).Init + (fst x) + (idx + 1)) % (snd x).Positions = 0)

let answer1 =
    Seq.initInfinite (fun x -> x + 1)
    |> Seq.filter (fun x -> isCollector input x)
    |> Seq.head

let input2 =
    input @
    [{ Positions = 11;
        Init = 0 }]

let answer2 =
    Seq.initInfinite (fun x -> x + 1)
    |> Seq.filter (fun x -> isCollector input2 x)
    |> Seq.head
