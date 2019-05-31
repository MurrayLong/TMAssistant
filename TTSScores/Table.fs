module Table
open Transform
open Vector

type SnapPoint = float32*float32*float32 


type GameObject = { Name: string; 
                    Nickname: string; 
                    GUID: string; 
                    Transform: Transform; 
                    AttachedSnapPoints: SnapPoint[]
                    }
type Hand = { Color: string; Transform:Transform}
type Hands = { HandTransforms: Hand[] } 
type Save = { 
    SaveName:string; GameMode:string; Hands: Hands; 
    ObjectStates: GameObject[]; 
    SnapPoints:SnapPoint[] }
let findObject scene predicate = Array.filter predicate scene.ObjectStates
let findNick scene nick = findObject scene (fun v->v.Nickname = nick)
let findbyID scene id = findObject scene (fun v->v.GUID = id) |> Seq.head

let isOnSnapPoint snapPoint position = 
    let snapThreshold = 0.0001f;
    let squareDistaneToSnap (snap:SnapPoint) = horizontalDistaneSquare position snap
    (squareDistaneToSnap snapPoint) < snapThreshold

let getSnapPointIndex snapPoints position = 
    Seq.tryFindIndex (fun p->isOnSnapPoint p position) snapPoints

let onSnapPoint (scene:Save) (x:GameObject) = 
    getSnapPointIndex scene.SnapPoints x.Transform.Translation

let localSnapPoints findObject scene = 
    let parent = findObject scene
    parent.AttachedSnapPoints |> Seq.map parent.Transform.Apply

let globalSnapPoints scene = scene.SnapPoints