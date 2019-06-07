module Table
open Transform
open Vector

type SnapPoint = float32*float32*float32 

type GameObject = { Name: string; 
                    Nickname: string; 
                    GUID: string; 
                    Transform: Transform; 
                    AttachedSnapPoints: SnapPoint[];
                    Number: int
                    }

type Hand = { Color: string; Transform:Transform}

type Hands = { HandTransforms: Hand[] } 

type Save = { 
    SaveName:string; 
    GameMode:string; 
    Hands: Hands; 
    ObjectStates: GameObject[]; 
    SnapPoints:SnapPoint[] }

let findObject predicate scene = Seq.filter predicate scene.ObjectStates 
let findNick nick = findObject (fun v->v.Nickname = nick)
let findbyID id = findObject (fun v->v.GUID = id) >> Seq.head

let isOnSnapPoint snapPoint (object:GameObject) = 
    let snapThreshold = 0.0001f;
    let position = object.Transform.Translation
    let squareDistaneToSnap (snap:SnapPoint) = horizontalDistaneSquare position snap
    (squareDistaneToSnap snapPoint) < snapThreshold

let getSnapPointIndex snapPoints object = 
    Seq.tryFindIndex (fun p->isOnSnapPoint p object) snapPoints

let onSnapPoint  (x:GameObject) (scene:Save)= 
    getSnapPointIndex scene.SnapPoints x

let localSnapPoints findObject scene = 
    let parent = findObject scene
    parent.AttachedSnapPoints |> Seq.map parent.Transform.Apply

let globalSnapPoints scene = scene.SnapPoints