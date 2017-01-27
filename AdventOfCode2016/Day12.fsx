open System
open System.IO

let dir = "C:/Users/olath/OneDrive/Dokument/AdventOfCodeFSharp/AdventOfCodeFSharp"
let input = File.ReadAllLines (dir + "/Day12Input.txt")

let testinput = 
    ["cpy 41 a";"inc a";"inc a";"dec a";"jnz a 2";"dec a"]
    |> Seq.toArray

let (|Int|_|) x = 
  match Int32.TryParse x with
  | true, n -> Some n
  | _       -> None

let Execute (str: string[]) = 
    let registers = new System.Collections.Generic.Dictionary<string, int>()
 
 // For part 2 only
    registers.["c"] <- 1

    let rec executeLine lineNum =
        if lineNum >= str.Length then
            registers
        else
           //printfn "%s" str.[lineNum]
           match str.[lineNum].Split ' ' with
                | [|"cpy"; Int n; dest |] ->  
                    registers.[dest] <- n
                    executeLine (lineNum+1)
                | [|"cpy"; src; dest|] ->
                    registers.[dest] <- registers.[src]
                    executeLine (lineNum+1)
                | [|"inc"; dest|] ->
                    registers.[dest] <- registers.[dest] + 1 
                    executeLine (lineNum+1)
                | [|"dec"; dest|] ->
                    registers.[dest] <- registers.[dest] - 1 
                    executeLine (lineNum+1)
                | [|"jnz"; Int cond; Int step|] when cond <> 0 ->
                        executeLine (lineNum+step)
                | [|"jnz"; cond; Int step|]->   
                    match registers.TryGetValue cond with
                        | true, n when n <> 0 -> executeLine (lineNum+step)
                        | _ -> executeLine (lineNum+1)
                | _ -> executeLine (lineNum+1)
            
    executeLine 0

let part1 = 
    Execute input

