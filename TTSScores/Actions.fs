module Actions
open TerraformingMars

let nextPlayers current (state:GameState) = 
    state.TurnSequence 
        |> List.skipWhile ( not <<(=) current )
        |> List.tail
        |> List.append (state.TurnSequence)

let nextStartPlayer (state:GameState) = 
    nextPlayers state.StartPlayer state |> Seq.head

let private generateResources (playerState:PlayerState) resource state = 
    match resource with
        | MCr   -> { state with stockpile = state.stockpile + state.income + playerState.TR }
        | Power -> { state with stockpile = state.income }
        | Heat  -> { state with stockpile = state.stockpile + state.income }
        | _     -> { state with stockpile = state.stockpile+state.income }
    

let private generatePlayer player state = 
    {state with Resources = state.Resources |> Map.map (generateResources state) }

let generation (state:GameState) = 
    let StartPlayer = nextStartPlayer state
    { 
        state with 
            Players = state.Players |> Map.map generatePlayer
            Generation = state.Generation |> Option.map ((+) 1)
            StartPlayer = StartPlayer
            CurrentPlayer = StartPlayer
    }
