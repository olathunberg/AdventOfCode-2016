open System
open System.IO

let dir = "C:/Users/olath/OneDrive/Dokument/AdventOfCodeFSharp/AdventOfCodeFSharp"
let input = File.ReadAllLines (dir + "/Day23Input.txt")

let testinput =
    ["cpy 2 a";"tgl a";"tgl a";"tgl a";"cpy 1 a";"dec a";"dec a"]
    |> Seq.toArray

let (|Int|_|) x =
  match Int32.TryParse x with
  | true, n -> Some n
  | _       -> None

let toggle (str:string) =
    match str.Split ' ' with
         | [|"tgl"; dest|] ->
            sprintf "inc %s" dest
         | [|"cpy"; rest1; rest2 |] ->
            sprintf "jnz %s %s" rest1 rest2
         | [|"inc"; dest|] ->
            sprintf "dec %s" dest
         | [|"dec"; dest|] ->
            sprintf "inc %s" dest
         | [|"jnz"; rest1; rest2|] ->
            sprintf "cpy %s %s" rest1 rest2
         | _ -> ""

let Execute (inStr: string[]) init =
    let registers = new System.Collections.Generic.Dictionary<string, int>()
    let str = inStr
    registers.["a"] <- init

    let rec executeLine lineNum =
        if lineNum >= str.Length then
            registers
        else
           //printfn "%s" str.[lineNum]
           match str.[lineNum].Split ' ' with
                | [|"tgl"; dest|] ->
                    let tglDest = lineNum+registers.[dest]
                    if tglDest < str.Length then
                        str.[tglDest] <- toggle str.[tglDest]
                    executeLine (lineNum+1)
                | [|"cpy"; Int n; Int r |] ->
                    executeLine (lineNum+1)
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
                | [|"jnz"; cond; Int step|] ->
                    match registers.TryGetValue cond with
                        | true, n when n <> 0 -> executeLine (lineNum+step)
                        | _ -> executeLine (lineNum+1)
                | [|"jnz"; Int cond; step|] ->
                    if cond <> 0 then
                        match registers.TryGetValue step with
                            | true, n -> executeLine (lineNum+n)
                            | _ -> executeLine (lineNum+1)
                    else
                        executeLine (lineNum+1)
                | _ -> executeLine (lineNum+1)

    executeLine 0

let part1 =
    Execute input 7

let part2 =
    Execute input 12
