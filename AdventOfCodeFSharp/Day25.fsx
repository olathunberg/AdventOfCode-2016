open System

let input =
    ["cpy a d";
     "cpy 4 c";
     "cpy 643 b";
     "inc d";
     "dec b";
     "jnz b -2";
     "dec c";
     "jnz c -5";
     "cpy d a";
     "jnz 0 0";
     "cpy a b";
     "cpy 0 a";
     "cpy 2 c";
     "jnz b 2";
     "jnz 1 6";
     "dec b";
     "dec c";
     "jnz c -4";
     "inc a";
     "jnz 1 -7";
     "cpy 2 b";
     "jnz c 2";
     "jnz 1 4";
     "dec b";
     "dec c";
     "jnz 1 -4";
     "jnz 0 0";
     "out b";
     "jnz a -19";
     "jnz 1 -21"]
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

let Execute (inStr: string[]) init testlength =
    let registers = new System.Collections.Generic.Dictionary<string, int>()
    let str = inStr
    let mutable result = ""
    registers.["a"] <- init

    let rec executeLine lineNum =
        if result.Length >= testlength then
            result
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
                | [|"out"; source|] ->
                    result <- sprintf "%s%d" result registers.[source]

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

// Test med 200 visar ett tydligt upprepande mönster om 12
let part1 =
    let testLength = 12
    let expected = [1..testLength] |> Seq.map (fun c -> sprintf "%d" ((c+1) % 2)) |> Seq.reduce (+)
    let rec test n =
        let result = Execute input n testLength
        printfn "%s" result
        if result = expected then
            n
        else
            test (n+1)

    test 0

