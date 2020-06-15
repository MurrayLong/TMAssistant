module Output.ScoreBot
open TerraformingMars
open Table

module Option =
    let zip a b =  
        match (a,b) with 
            | (Some x, Some y) -> Some (x,y)
            | _ -> None

type Property = {
    player: Player option;
    name: string;
    value: int;
}

let globalProperties : (string*(GameState->int option)) list =
    [
        ("gen", fun f -> f.Generation)
        ("temp", fun f -> f.Temp)
        ("oxy", fun f -> f.O2)
        ("lake", fun f -> Some f.Oceans)
    ]

let private playerName = function
  | Green -> "Murray"
  | Red -> "Sam"
  | Yellow -> "Ross"
  | _-> "Not Playing"

let private isPlaying = function
    | Green -> true
    | Red -> true
    | Yellow -> true
    | _ -> false

let playerProperties : (string*(PlayerState->int option)) list=
    let income ``type`` state = 
        state.Resources |> Map.tryFind ``type``  |> Option.map (fun v->v.income) 
    let current ``type`` state = 
        state.Resources |> Map.tryFind ``type``  |> Option.map (fun v->v.stockpile) 
    [
        ("tr", fun f -> Some f.TR)
        ("$",  current MCr)
        ("$$", income MCr)
        ("s",  current Steal)
        ("sp", income Steal)
        ("t",  current Titanium)
        ("tp", income Titanium)
        ("g",  current Plants)
        ("gp", income Plants)
        ("p",  current Power)
        ("pp", income Power)
        ("h",  current Heat)
        ("hp", income Heat)
    ]

let private getProperty (player, state) (name,f) = 
    f state |> Option.map (fun value ->
        {
            player= player
            value = value
            name=name
        })

let getPlayerProperties (player,state) = 
    playerProperties 
        |> List.map (getProperty (Some player,state))

let getProperties gameState = 
    let globals = globalProperties 
                    |> List.map (getProperty (None,gameState))

    let players = gameState.Players 
                    |> Map.toList 
                    |> List.filter (fun (a,b)->isPlaying a)
                    |> List.collect getPlayerProperties

    List.concat [ globals; players ]

let diffSingle pa pb = 
    match (pa,pb) with
        | (Some a , Some b) when not (a.value = b. value) -> Some { a with value = b.value-a.value}
        | _ -> None


let getDiff before after = 
    let propsBefore = getProperties before
    let propsAfter = getProperties after
    List.map2 diffSingle propsBefore propsAfter 
        |> List.choose id
