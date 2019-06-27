module Output.KeyValuePairs
open TerraformingMars
open Table

let snd (a,b)=b

let private formatIntOption = function
  | Some x -> sprintf "%i" x
  | None -> "[UNKNOWN]" 

let private playerName = function
  | Green -> "Murray"
  | Red -> "Sam"
  | Yellow -> "Ross"
  | _-> "Not Playing"

let private resourceValues resource (resourceState:ResourceState) = 
    [
        ("", resourceState.income.ToString());
        ("stockpile", resourceState.stockpile.ToString())
    ]

let private appendResourceName (resource:Resource) (fields)= 
    fields |> List.map (fun (k,v)->((resource.ToString())+" "+k,v))

let private resourcesValues resourceState : (string*string)list= 
    resourceState 
        |> Map.map resourceValues
        |> Map.map appendResourceName
        |> Map.toList
        |> List.map snd
        |> List.concat


let private playerValues player (playerState:PlayerState): (string*string)list = 
    List.concat
        [ 
            [ ("TR", playerState.TR.ToString()) ];
            resourcesValues playerState.Resources
        ] 

let private appendName (player:Player) (fields)= 
    fields |> List.map (fun (k,v)->((player.ToString())+"'s "+k,v))

let private playersValues state : (string*string) list= 
    state.Players 
        |> Map.map playerValues 
        |> Map.map appendName
        |> Map.toList
        |> List.map snd
        |> List.concat

let keyValuePairs (state:GameState)=
    List.concat
        [[
            ("O2", formatIntOption state.O2);
            ("Generation",formatIntOption state.Generation);
            ("Temp",formatIntOption state.Temp);
            ("Start Player",state.StartPlayer.ToString());
            ("Current Player",state.CurrentPlayer.ToString());
            ("Next Player",formatIntOption state.O2);
            ("Oceans Placed",state.Oceans.ToString())
          ];
          playersValues state
        ]
    

let format (state:GameState)  = 
    sprintf "%A" (keyValuePairs state)