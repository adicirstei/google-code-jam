let toSeq s =
  seq {for c in s -> c}

let combineTwo a b =
  [for i in a do 
    for j in b do 
      if i < j then yield (i,j)]




let findAll (input:string) = 
  let searchString = [for c in "welcome to code jam" -> c ]
  let idxOf c =
    input
    |> toSeq
    |> Seq.mapi (fun i c -> (i,c))
    |> Seq.filter (fun (_, x) -> c = x)
    |> Seq.map fst

  searchString
  |> List.map idxOf
  

findAll "wellcoome to coodee jamm"


let interact (fn:(string -> string)) : int = 
  System.Console.Out.Write(fn (System.Console.In.ReadToEnd()))
  0

module String =
  let Split by (input:string) = 
    input.Split [|by|]
    |> Array.toList

let solve i = 
  let (_ :: data) = String.Split '\n' i
  
















[<EntryPoint>]
let main _ = 
  interact solve


