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

type ResourceState = {Resource: Resource; Income: int; Stockpiled: int}

type Player = | Yellow | Green | Red | Blue | Black
with 
    member this.ToString() = match this with 
        | Yellow -> "Yellow"
        | Green -> "Green"
        | Red -> "Red"
        | Blue -> "Blue"
        | Black -> "Black"
    static member All = [| Yellow ; Green ; Red ; Blue ; Black |]

let objectsNamed name scene = scene.ObjectStates |> Array.filter (fun g->g.Nickname=name)
let objectWithID id scene = scene.ObjectStates |> Seq.filter (fun g->g.GUID=id) 

let markersFor player = objectsNamed <| sprintf "%A Cube" player 
let boardFor player = (objectsNamed <| sprintf "%A Board" player) >> Seq.head

[<StructuredFormatDisplay("{AsString}")>]
type GameState = { 
        O2: int option; 
        Temp: int option;
        TR: Map<Player, int option>
        } with
    
    member this.TRString = 
        let TRForPlayer p = Map.find p this.TR
        let TRForPlayerString (p:Player) = sprintf "%s: %A, " (p.ToString()) (TRForPlayer p)
        Player.All  |> Seq.map TRForPlayerString 
                    |> Seq.fold (+) ""

    override this.ToString() 
        = sprintf "O2: %A  Temp: %A C TR: %s" this.O2 this.Temp this.TRString

    member this.AsString = this.ToString()  

let otherwise x o = match o with
                    | Some a -> a
                    | None -> x
                

let scoreTrack snapPoints markers scene =
    let snapIndex (x:GameObject) = getSnapPointIndex (snapPoints scene) x.Transform.Translation
    scene |> markers 
          |> Seq.map snapIndex
          |> Seq.map (otherwise 0)
          |> Seq.fold (+) 0

let rec between (a,b) = 
    if (a>b) then between (b,a)
    else Seq.skip(a) >> Seq.take(1+(b-a))

let CRIncome player = 
    let snapPoints =  (localSnapPoints (boardFor player)) >> between (9,100)
    scoreTrack snapPoints (markersFor player)

let simpleCounter guid offset scene = 
    let marker = findbyID scene guid 
    let snap = onSnapPoint scene marker
    Option.map (fun v->v-offset) snap 

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

let interpret scene =
    {
        O2 = O2Level scene;
        Temp = TempLevel scene;
        TR = Player.All |> Array.map (fun p->(p,TRMarker p scene)) 
                        |> Map.ofArray
    }

let loadFile file = TTSJson.loadScene file |> interpret

let TRString state = 
    let TRForPlayer p = Map.find p state.TR
    let TRForPlayerString (p:Player) = sprintf """ %s: %A""" (p.ToString()) (TRForPlayer p)
    Player.All  |> Seq.map TRForPlayerString 
                |> Seq.fold (+) ""

let format state  = 
    sprintf """O2: %A%%
Temp: %A c
TR: %s
    """ state.O2 state.Temp (TRString state)

