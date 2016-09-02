type InteractCallback = (string -> string)

let interact (fn:InteractCallback) : unit = 
  System.Console.Out.Write(fn (System.Console.In.ReadToEnd()))

 
let parseHeader (line: string) =
  let tokens  = line.Split [|' '|]
  match tokens with
  | [|a;b;c|] -> (int a, int b, int c)
  | _ -> failwith "invalid header"

let solveTest d len n t =
  sprintf "Case #%d: %d" n (String.length t)

let join a b = 
  a + "\n" + b

let solve (input:string) = 
  let h::tail = Array.toList (input.Split [|'\n'|])
  
  let (l,d,n) = parseHeader h
  let dict = 
    List.take d tail
    |> Set.ofList

  let tests = List.skip d tail

  Seq.map2 (solveTest dict l) [1..n] tests
  |> Seq.reduce join
  

[<EntryPoint>]
let main _ = 
  interact solve
  0