module TTSJson
open System
open Enviroment
open Vector

let otherwise a o = match o with
                    | Some x -> x
                    | None -> a

type Position = {x:float32;y:float32;z:float32}

type _snapPoint = Table.SnapPoint
type SnapPoint = {Position: Position} 
    with member this.Convert() = (this.Position.x, this.Position.y, this.Position.z)

type _transform = Transform.Transform
type Transform = { 
    posX:float32; posY:float32; posZ:float32; 
    rotX:float32; rotY:float32; rotZ:float32; 
    scaleX:float32; scaleY:float32; scaleZ:float32; 
} with member this.Convert() =  {
                                _transform.Translation = (this.posX, this.posY, this.posZ);
                                _transform.Rotation = (this.rotX, this.rotY, this.rotZ);
                                _transform.Scale = (this.scaleX, this.scaleY, this.scaleZ)
                                } 

    

type _gameObject = Table.GameObject
type GameObject = { Name: string; 
                    Nickname: string; 
                    GUID: string; 
                    Transform: Transform; 
                    AttachedSnapPoints: Option<SnapPoint[]>;
                    Number: int;
                    }
    with member this.Convert() = {
        _gameObject.Name=this.Name;
        _gameObject.Nickname=this.Nickname;
        _gameObject.GUID=this.GUID;
        _gameObject.Transform=this.Transform.Convert();
        _gameObject.Number=this.Number;
        _gameObject.AttachedSnapPoints = this.AttachedSnapPoints 
                    |> otherwise Array.empty
                    |> Array.map (fun v->v.Convert())
    }
type _hand = Table.Hand
type Hand = { Color: string; Transform:Transform}
    with member this.Convert() = {
        _hand.Color=this.Color;
        _hand.Transform=this.Transform.Convert()
    }
type _hands = Table.Hands
type Hands = { HandTransforms: Hand[] } 
    with member this.Convert() = {
        _hands.HandTransforms = this.HandTransforms |> Array.map (fun f->f.Convert())
    }

type _save = Table.Save
type Save = { SaveName:string; GameMode:string; Hands: Hands; ObjectStates: GameObject[]; SnapPoints:SnapPoint[] option }
    with member this.Convert() = {
        _save.SaveName=this.SaveName;
        _save.GameMode=this.GameMode;
        _save.Hands=this.Hands.Convert();
        _save.ObjectStates=this.ObjectStates |> Array.map (fun v->v.Convert());
        _save.SnapPoints=this.SnapPoints 
                    |> otherwise Array.empty 
                    |> Array.map (fun w->w.Convert());
        }

let load json = 
    let convert (a:Save) = a.Convert()
    json |> ofJson<Save> |> convert
    
let loadScene fileName = 
    let convert (a:Save) = a.Convert()
    readFile fileName |> ofJson<Save> |> convert



