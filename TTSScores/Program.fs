// Learn more about F# at http://fsharp.org

open System
open Newtonsoft.Json
open TerraformingMars
open Table

let prettyJson obj = JsonConvert.SerializeObject(obj, Formatting.Indented)

let playerName = function
  | Green -> "Murray"
  | Red -> "Sam"
  | Yellow -> "Ross"
  | _-> "Not Playing"

let formatIntOption = function
  | Some x -> sprintf "%i" x
  | None -> "[UNKNOWN]" 

let formatResource (resource:Resource) (values:ResourceState) = 
  let name = resource.ToString()
  sprintf "  %6s: %3i %+i" name values.stockpile values.income

let formatPlayer (player:Player) (state:PlayerState) = 
  sprintf "%A
  TR: %i%s
  " (playerName player) state.TR (state.Resources |> Map.map (formatResource) |> Map.fold (fun a _ b -> a+"\r\n"+b) "")
  

let format (state:GameState)  = 
    sprintf """ 
WE ARE WAITING FOR: %s    
Generation %s 
Start Player: %s
O2: %s%%
Temp: %s c
Oceans placed %i/9
%s
    """ 
        (playerName state.CurrentPlayer)
        (formatIntOption state.Generation) 
        (playerName state.StartPlayer)
        (formatIntOption state.O2) 
        (formatIntOption state.Temp) 
        state.Oceans
        (state.Players |> Map.map (formatPlayer) |> Map.fold (fun a _ b -> a+"\r\n\r\n"+b) "")

let snapPointIndexUnderID id scene = 
        let marker = Table.findbyID id scene 
        let snap = Table.onSnapPoint marker scene 
        snap |> printfn "Snap Point %A"

let test a = [0]

let textYellowBoard (scene:Save) = 
  let board = playerBoard Yellow scene |> Seq.head
  let snaps = board.AttachedSnapPoints

  let translatedSnaps = seq {
    for s in snaps do
      yield board.Transform.Apply s
  }

  let cube = findbyID "b341ac" scene
  let snap = getSnapPointIndex translatedSnaps cube
  printf "Index %A %A" snap snaps.[snap|>Option.defaultValue -1]
  snap

[<EntryPoint>]
let main argv =
(*
  let scene = TTSJson.loadScene """C:\Users\murray.long\Workspace\TTSScores\Examples\terraformingmarsExample.json"""
  let board = findNick "Yellow Board" scene |> Seq.head
  let debug = findNick "Debug" scene |> Seq.head
  let boundry = stockPilesBounds |> Map.find MCr 
  printfn  "%A" boundry
  printfn "debug position on board: %A" (board.Transform.Reverse debug.Transform.Translation)
  printfn "%A" <| WithinBoundry board boundry debug
  0
  *)
  let scene = stdin.ReadToEnd() |> TTSJson.load 
  scene |> TerraformingMars.interpret
        |> format 
        |> Console.WriteLine 
  0
