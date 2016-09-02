type ConsoleInteract = (string -> string)

let interact (fn:ConsoleInteract) : unit = 
  System.Console.Out.Write(fn (System.Console.In.ReadToEnd()))















[<EntryPoint>]
let main _ = 
  interact (sprintf "...%s...") |> ignore
  0