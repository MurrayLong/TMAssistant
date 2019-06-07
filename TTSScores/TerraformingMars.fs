(* TODO:
    Make score tracks more efficient by filtering before checking location
*)

module TerraformingMars
open Table

let phobos = 96
let ganymede = 97
//mars 0 .. 59 (top row) snaking

(* 
198 - Terraformer
199 - Mayor
Gardener
Builder
Planner
*)

(*
203 - Landlord
204 - Banker
Scientist
Thermalist
Miner

*)


type Resource = | MCr | Steal | Titanium | Plants | Power | Heat

type Player = | Yellow | Green | Red | Blue | Black
with 
    member this.ToString() = match this with 
        | Yellow -> "Yellow"
        | Green -> "Green"
        | Red -> "Red"
        | Blue -> "Blue"
        | Black -> "Black"
    static member All = [| Yellow ; Green ; Red ; Blue ; Black |]

type ResourceState = {
  income: int;
  stockpile: int
}

type PlayerState = {
  TR: int ;
  Resources: Map<Resource, ResourceState>
}

[<StructuredFormatDisplay("{AsString}")>]
type GameState = { 
        O2: int option; 
        Temp: int option;
        Oceans: int;
        Players: Map<Player, PlayerState>
        } 


let objectsNamed name scene = scene.ObjectStates |> Array.filter (fun g->g.Nickname=name)
let objectWithID id scene = scene.ObjectStates |> Seq.filter (fun g->g.GUID=id)

let markersFor player = objectsNamed <| sprintf "%A Cube" player 
let boardFor player = (objectsNamed <| sprintf "%A Board" player) >> Seq.head

let scoreTrack snapPoints markers scene =
    let snapIndex (x:GameObject) = getSnapPointIndex (snapPoints scene) x
    scene |> markers 
          |> Seq.map snapIndex
          |> Seq.map (Option.defaultValue 0)
          |> Seq.fold (+) 0

let rec between (a,b) = 
    if (a>b) then between (b,a)
    else Seq.skip(a) >> Seq.take(1+(b-a))

let playerBoard = function
    | Yellow -> objectWithID "414e30"
    | Red ->    objectWithID "b1cfa0"
    | Black ->  objectWithID "583d53"
    | Blue ->   objectWithID "75192e"
    | Green ->  objectWithID "31b4c1"

let CRIncome player = 
    let snapPoints = (localSnapPoints (boardFor player)) >> between (9,100)
    scoreTrack snapPoints (markersFor player)

let O2Level = 
    let scale x = 14-x
    scoreTrack (globalSnapPoints >> between (81,95)) (objectWithID "c8926e") >> scale >> Some

let oceanSnapIndex = 80

let TempLevel = 
    let scale x = 2*(x-15) 
    scoreTrack (globalSnapPoints >> between (60,79)) (objectWithID "5a3116") >> scale >> Some

let TRMarker player = 
    let scale x = if (x=0) then 100 else x
    scoreTrack (globalSnapPoints >> between (98,198)) (markersFor player) >> scale >> Some

let oceans scene = 
  let remaining = 
    scene.ObjectStates 
         |> Seq.filter (isOnSnapPoint (scene.SnapPoints.[oceanSnapIndex]))
         |> Seq.tryHead
         |> Option.map (fun v->if v.Number=0 then 1 else v.Number)
         |> Option.defaultValue 0
  9-remaining

let playerState player scene = 
  {
      TR = TRMarker player scene |> Option.defaultValue 0
      Resources = Map.empty 
    }

let interpret scene =
    {
        O2 = O2Level scene;
        Temp = TempLevel scene;
        Oceans = oceans scene;
        Players = Player.All 
                  |> Array.map (fun p -> p, playerState  p scene) 
                  |> Map.ofArray
    }

let loadFile file = TTSJson.loadScene file |> interpret

