open System
open System.IO

let indata =
    File.ReadAllLines
        ("C:\Users\olath\OneDrive\Dokument\Visual Studio 2017\Projects\AdventOfCodeFSharp\AdventOfCodeFSharp\Day08Input.txt")

let rect (arr : string [,]) (x : int) (y : int) =
    let ne = Array2D.create y x "#"
    Array2D.blit ne 0 0 arr 0 0 y x
    arr

let rotaterow (arr : string [,]) (y : int) (n : int) =
    let x = arr.GetLength(1) - n
    let rot = Array.append arr.[y, x..] arr.[y, ..(x - 1)]
    [ 0..arr.GetLength(1) - 1 ] |> Seq.iter (fun c -> Array2D.set arr y c rot.[c])
    arr

let rotatecolumn (arr : string [,]) (x : int) (n : int) =
    let y = arr.GetLength(0) - n
    let rot = Array.append arr.[y.., x] arr.[..(y - 1), x]
    [ 0..arr.GetLength(0) - 1 ] |> Seq.iter (fun c -> Array2D.set arr c x rot.[c])
    arr

let doMove (arr : string [,]) (cmd : string) =
    let split = cmd.Split ' '
    match split.[0] with
    | "rect" -> rect arr (Int32.Parse(split.[1].Split 'x' |> Seq.head)) (Int32.Parse(split.[1].Split 'x' |> Seq.last))
    | "rotate" ->
        match split.[1] with
        | "row" -> rotaterow arr (Int32.Parse(split.[2].Split '=' |> Seq.last)) (Int32.Parse(split |> Seq.last))
        | "column" -> rotatecolumn arr (Int32.Parse(split.[2].Split '=' |> Seq.last)) (Int32.Parse(split |> Seq.last))
        | _ -> arr
    | _ -> arr

let display = Array2D.create 6 50 " "

indata |> Seq.iter (fun c -> doMove display c |> ignore)
display
|> Seq.cast<string>
|> Seq.filter (fun x -> x = "#")
|> Seq.length

let getStringRow (arr:string[,]) (x:int) =
           arr.[x..x, *]
           |> Seq.cast<string>
           |> Seq.reduce (+)

let answer2 =
    [ 0..display.GetLength(0)-1]
    |> Seq.map (fun c -> getStringRow display c)
    |> Seq.iter (fun c -> printf "%s\n" c)
