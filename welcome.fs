
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


