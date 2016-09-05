type InteractCallback = (string -> string)

let interact (fn:InteractCallback) : unit = 
  System.Console.Out.Write(fn (System.Console.In.ReadToEnd()))

 
let parseHeader (line: string) =
  let tokens  = line.Split [|' '|]
  match tokens with
  | [|a;b;c|] -> (int a, int b, int c)
  | _ -> failwith "invalid header"

let (|St|_|) (c:string) (s:string) =
  if s.StartsWith(c) then Some (s.Substring(1))
  else None

let split (c:char) (s:string) =
  let i = s.IndexOf(c)
  if i = -1 then (s, "")
  else (s.[..(i-1)], s.[(i+1)..])

let flip f a b = f b a
let all = List.forall id

let join a b = 
  a + "\n" + b


let rec parse s = 
  match s with
  | "" -> []
  | St "(" rest -> 
    let (a,b) = split ')' rest
    [ for c in a ->c] :: (parse b)
  | x -> [x.[0]] :: (parse x.[1..])



let solveTest dict idx test =
  let possibleWord =
    all << (List.map2 (flip List.contains) (parse test))

  let sol = 
    dict
    |> List.map (fun s -> [for c in s -> c])
    |> List.filter possibleWord
    |> List.length
  sprintf "Case #%d: %d" idx sol


let solve (input:string) = 
  let h::tail = Array.toList (input.Split [|'\n'|])
  
  let (l,d,n) = parseHeader h
  let dict = 
    List.take d tail

  let tests = List.skip d tail

  Seq.map2 (solveTest dict) [1..n] tests
  |> Seq.reduce join

[<EntryPoint>]
let main _ = 
  interact solve
  0