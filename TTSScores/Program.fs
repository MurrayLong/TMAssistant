// Learn more about F# at http://fsharp.org

open System
open TTSJson
open Newtonsoft.Json
open TerraformingMars
open Table

let prettyJson obj = JsonConvert.SerializeObject(obj, Formatting.Indented)

let formatIntOption = function
  | Some x -> sprintf "%i" x
  | None -> "[UNKNOWN]" 

let formatResource (resource:Resource) (values:ResourceState) = 
  let name = resource.ToString()
  sprintf "  %s: %i + %i" name values.stockpile values.income

let formatPlayer (player:Player) (state:PlayerState) = 
  sprintf "%A
  TR: %i%s
  " player state.TR (state.Resources |> Map.map (formatResource) |> Map.fold (fun a _ b -> a+"\r\n"+b) "")
  

let format (state:GameState)  = 
    sprintf """Generation %s 
O2: %s%%
Temp: %s c
Oceans placed %i/9
%s
    """ (formatIntOption state.Generation) 
        (formatIntOption state.O2) 
        (formatIntOption state.Temp) 
        state.Oceans
        (state.Players |> Map.map (formatPlayer ) |> Map.fold (fun a _ b -> a+"\r\n\r\n"+b) "")

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

  let cube = findbyID "4d5291" scene
  getSnapPointIndex translatedSnaps cube

[<EntryPoint>]
let main argv =
(*
  let scene = stdin.ReadToEnd() |> TTSJson.load 
  textYellowBoard scene |> printfn "Snap Index: %A" |> ignore
  let value = TerraformingMars.CRIncome Yellow scene
  printfn "Yellow CR Income %i" value
  0
  *)
    stdin.ReadToEnd()
        |> TTSJson.load 
        |> TerraformingMars.interpret
        |> format 
        |> Console.WriteLine 
    0
