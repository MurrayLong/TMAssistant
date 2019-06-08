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
with 
    member this.ToString() = match this with 
        | MCr -> "MCr"
        | Steal -> "Steal"
        | Titanium -> "Ti"
        | Plants -> "Plants"
        | Power -> "Power"
        | Heat -> "Heat"

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
        Generation: int option;
        O2: int option; 
        Temp: int option;
        Oceans: int;
        Players: Map<Player, PlayerState>
        } 


let objectsNamed name scene = scene.ObjectStates |> Array.filter (fun g->g.Nickname=name)
let objectWithID id scene = scene.ObjectStates |> Seq.filter (fun g->g.GUID=id)

let markersFor player = objectsNamed <| sprintf "%A Cube" player 

let scoreTrack snapPoints markers scale scene =
    let snapIndex (x:GameObject) = getSnapPointIndex (snapPoints scene) x
    scene |> markers 
          |> Seq.map snapIndex
          |> Seq.choose id
          |> Seq.map scale
          |> Seq.fold (+) 0

let rec between (a,b) = 
    if (a>b) then between (b,a)
    else Seq.skip(a) >> Seq.take(1+(b-a))

let selectIndexes indexes l = 
  let arr = l |> Seq.toArray
  seq {
    for i in indexes do 
    yield arr.[i]
  }

let playerBoard = function
    | Yellow -> objectWithID "414e30" 
    | Red ->    objectWithID "b1cfa0" 
    | Black ->  objectWithID "583d53" 
    | Blue ->   objectWithID "75192e" 
    | Green ->  objectWithID "31b4c1" 

let CRIncome player scene = 
    let scale x = x-5
    let board = playerBoard player >> Seq.head
    let snapPoints = (localSnapPoints board) >> selectIndexes [0;1;2;3;4;10;9;8;7;6;5;11;12;13;14;15]
    scoreTrack snapPoints (markersFor player) scale scene

let steelIncomeIndexes = [32;31;30;29;28;27; 33;34;35;36;37]
let tiIncomeIndexes    = [38;39;40;41;42;43; 48;47;46;45;44]
let plantIncomeIndexes = [47;20;19;18;17;16; 22;23;24;25;26]
let powerIncomeIndexes = [70;68;69;63;62;61; 67;66;64;65;60]
let heatIncomeIndexes  = [49;50;51;52;53;54; 59;58;57;56;55]

let SteelIncome player scene =
    let board = playerBoard player >> Seq.head
    let snapPoints = (localSnapPoints board) >> selectIndexes steelIncomeIndexes
    scoreTrack snapPoints (markersFor player) id scene

let TiIncome player scene =
    let board = playerBoard player >> Seq.head
    let snapPoints = (localSnapPoints board) >> selectIndexes tiIncomeIndexes
    scoreTrack snapPoints (markersFor player) id scene

let PlantIncome player scene =
    let board = playerBoard player >> Seq.head
    let snapPoints = (localSnapPoints board) >> selectIndexes plantIncomeIndexes
    scoreTrack snapPoints (markersFor player) id scene

let PowerIncome player scene =
    let board = playerBoard player >> Seq.head
    let snapPoints = (localSnapPoints board) >> selectIndexes powerIncomeIndexes
    scoreTrack snapPoints (markersFor player) id scene

let HeatIncome player scene =
    let board = playerBoard player >> Seq.head
    let snapPoints = (localSnapPoints board) >> selectIndexes steelIncomeIndexes
    scoreTrack snapPoints (markersFor player) id scene

let Generation = 
    let scale x = if (x=0) then 100 else x
    scoreTrack (globalSnapPoints >> between (98,198)) (objectWithID "5dc1b8") scale >> Some

let O2Level = 
    let scale x = 14-x
    scoreTrack (globalSnapPoints >> between (81,95)) (objectWithID "c8926e") scale >> Some

let oceanSnapIndex = 80

let TempLevel = 
    let scale x = 2*(x-15) 
    scoreTrack (globalSnapPoints >> between (60,79)) (objectWithID "5a3116") scale >> Some

let TRMarker player = 
    let scale x = if (x=0) then 100 else x
    scoreTrack (globalSnapPoints >> between (98,198)) (markersFor player) scale >> Some

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
      Resources = [ 
                (MCr, { income= CRIncome player scene;  stockpile=0});
                (Steal, { income= SteelIncome player scene;  stockpile=0});
                (Titanium, { income= TiIncome player scene;  stockpile=0});
                (Plants, { income= PlantIncome player scene;  stockpile=0});
                (Power, { income= PowerIncome player scene;  stockpile=0});
                (Heat, { income= HeatIncome player scene;  stockpile=0});
          ] |> Map.ofSeq
    }

let interpret (scene:Save) =
    {
        Generation = Generation scene;
        O2 = O2Level scene;
        Temp = TempLevel scene;
        Oceans = oceans scene;
        Players = Player.All 
                  |> Array.map (fun p -> p, playerState  p scene) 
                  |> Map.ofArray
    }

let loadFile file = TTSJson.loadScene file |> interpret
