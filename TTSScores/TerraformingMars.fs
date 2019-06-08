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
        | Plants -> "Plant"
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

type StockPileBoundry ={
    TopLeft: float32*float32*float32;
    BottomRight: float32*float32*float32;
 }

let stockPilesBounds = Map.ofList [
  (MCr, {
    TopLeft= (1.28126335f, 0.325000525f, -0.91987294f);
    BottomRight = (0.210445493f, 0.325000525f, -0.346702665f);
  });
  (Steal, {
    TopLeft = (0.174996525f, 0.325000525f, -0.914289296f);
    BottomRight =(-0.533730447f, 0.325000525f, -0.346435606f)
  });
  (Titanium, {
    TopLeft = (-0.57846868f, 0.325000525f, -0.911780357f);
    BottomRight = (-1.28582084f, 0.325000525f, -0.34839645f)
  });
  (Heat, {
    TopLeft = (-0.441874564f, 0.325000525f, 0.419090182f);
    BottomRight = (-1.31089461f, 0.325000525f, 0.921986401f)
  });
  (Power, {
    TopLeft =  (0.359622389f, 0.325000525f, 0.412322611f);
    BottomRight= (-0.389986277f, 0.325000525f, 0.918745995f)
  })
  (Plants, {
    TopLeft= (1.28476608f, 0.325000525f, 0.406576335f);
    BottomRight = (0.393995911f, 0.325000525f, 0.927631259f)
  });
  ]

let WithinBoundry (board:GameObject) (boundry:StockPileBoundry) (obj:GameObject) = 
  let (x,y,z) = board.Transform.Reverse obj.Transform.Translation
  let (left,_,top) = boundry.TopLeft
  let (right,_,bottom) = boundry.BottomRight
  x<left && x>right && z > top && z<bottom


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
    | Yellow -> objectWithID "efa425" //"414e30" 
    | Red ->    objectWithID "b1cfa0" 
    | Black ->  objectWithID "583d53" 
    | Blue ->   objectWithID "75192e" 
    | Green ->  objectWithID "31dd6a" 

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
    let snapPoints = (localSnapPoints board) >> selectIndexes heatIncomeIndexes
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

let cubeValue (cube:GameObject) = 
  match cube.Nickname with
    | "1" -> 1
    | "5" -> 5
    | "10" -> 10
    | _ ->0

let stocksOf resource player scene = 
  let board = playerBoard player scene |> Seq.head
  let boundry = stockPilesBounds 
                  |> Map.tryFind resource 
                  |> Option.defaultValue {TopLeft= (0.0f,0.0f,0.0f); BottomRight=(0.0f,0.0f,0.0f)}
  scene.ObjectStates 
    |> Array.filter (WithinBoundry board boundry) 
    |> Seq.sumBy cubeValue 

let playerState player scene = 
  {
      TR = TRMarker player scene |> Option.defaultValue 0
      Resources = [ 
                (MCr, { income= CRIncome player scene;  stockpile=stocksOf MCr player scene });
                (Steal, { income= SteelIncome player scene;  stockpile=stocksOf Steal player scene});
                (Titanium, { income= TiIncome player scene;  stockpile=stocksOf Titanium player scene});
                (Plants, { income= PlantIncome player scene;  stockpile=stocksOf Plants player scene});
                (Power, { income= PowerIncome player scene;  stockpile=stocksOf Power player scene});
                (Heat, { income= HeatIncome player scene;  stockpile=stocksOf Heat player scene});
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
