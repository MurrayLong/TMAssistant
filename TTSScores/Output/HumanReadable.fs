module Output.HumanReadable
open TerraformingMars
open Table

let private playerName = function
  | Green -> "Murray"
  | Red -> "Sam"
  | Yellow -> "Ross"
  | _-> "Not Playing"

let private formatIntOption = function
  | Some x -> sprintf "%i" x
  | None -> "[UNKNOWN]" 

let private formatResource (resource:Resource) (values:ResourceState) = 
  let name = resource.ToString()
  sprintf "  %6s: %3i %+i" name values.stockpile values.income

let private formatPlayer (player:Player) (state:PlayerState) = 
  sprintf "%A
  TR: %i%s
  " (playerName player) state.TR (state.Resources |> Map.map (formatResource) |> Map.fold (fun a _ b -> a+"\r\n"+b) "")
  

let format (state:GameState)  = 
    sprintf """ 
WE ARE WAITING FOR: %s    
Generation %s 
Start Player: %s
O2: %s%%
Temp: %s c
Oceans placed %i/9
%s
    """ 
        (playerName state.CurrentPlayer)
        (formatIntOption state.Generation) 
        (playerName state.StartPlayer)
        (formatIntOption state.O2) 
        (formatIntOption state.Temp) 
        state.Oceans
        (state.Players |> Map.map (formatPlayer) |> Map.fold (fun a _ b -> a+"\r\n\r\n"+b) "")