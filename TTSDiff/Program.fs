// Learn more about F# at http://fsharp.org

open System
open Argu
open System.IO
open TerraformingMars
open System.Threading

module Option =
    let zip a b =  
        match (a,b) with 
            | (Some x, Some y) -> Some (x,y)
            | _ -> None

let rec waitToRead fileName = 
    try 
        File.ReadAllText(fileName) 
    with 
       | :? System.IO.IOException ->
                Thread.Sleep 100
                waitToRead fileName

let loadState fileName = 
    waitToRead fileName
        |> TTSJson.load 
        |> TerraformingMars.interpret

let printState = Output.HumanReadable.format >> Console.WriteLine

let listenForChanges (fileName:string) f = 
    let fileSystemWatcher = new FileSystemWatcher()

    fileSystemWatcher.Path <- Path.GetDirectoryName fileName
    fileSystemWatcher.Filter <- Path.GetFileName fileName
    fileSystemWatcher.EnableRaisingEvents <- true
    fileSystemWatcher.Changed.Add(fun _ -> f())

let globalProperties : (string*(GameState->int option)) list =
    [
        ("gen", fun f -> f.Generation)
        ("temp", fun f -> f.Temp)
        ("oxy", fun f -> f.O2)
        ("lake", fun f -> Some f.Oceans)
    ]

let private playerName = function
  | Green -> "Murray"
  | Red -> "Sam"
  | Yellow -> "Ross"
  | _-> "Not Playing"

let playerProperties : (string*(PlayerState->int option)) list=
    let income ``type`` state = 
        state.Resources |> Map.tryFind ``type``  |> Option.map (fun v->v.income) 
    let current ``type`` state = 
        state.Resources |> Map.tryFind ``type``  |> Option.map (fun v->v.stockpile) 
    [
        ("tr", fun f -> Some f.TR)
        ("$",  current MCr)
        ("$$", income MCr)
        ("s",  current Steal)
        ("sp", income Steal)
        ("t",  current Titanium)
        ("tp", income Titanium)
        ("g",  current Plants)
        ("gp", income Plants)
        ("p",  current Power)
        ("pp", income Power)
        ("h",  current Heat)
        ("hp", income Heat)
    ]

let getPlayerProperties (player,state) = 
    playerProperties |> List.map (fun (name,f) -> ((playerName player)+" "+name, f state))

let getProperties gameState = 
    let globals = globalProperties |> List.map (fun (name,f) -> (name,f gameState))
    let players = gameState.Players 
                    |> Map.toList 
                    |> List.collect getPlayerProperties

    List.concat [ globals; players ]

let diffSingle (name,valueA) (_,valueB) = 
    Option.zip valueA valueB 
        |> Option.map (fun (a,b)->(name, b-a))

let getDiff before after = 
    let propsBefore = getProperties before
    let propsAfter = getProperties after
    List.map2 diffSingle propsBefore propsAfter 
        |> List.choose id

let printDiff diff = 
    let changed = diff |> List.filter (fun (_,change)->not (change =0))
    if (List.length changed = 0) then
        printfn "NONE YET"
    else
        printfn "CUMULATIVE CHANGE"
        for (name, change) in changed do
            printfn "/Score @ %s:%i" name change

let onChange initialState fileName () = 
    printfn "FILE UPDATED"
    printfn "NEW STATE:"
    let newState = loadState fileName
    printState newState
    printfn "Changes"
    printDiff <| getDiff initialState newState
    printfn "----------------"

let monitorFile fileName = 
    printfn "Monitoring file %s" fileName
    printfn "Initial State"
    let initialState = loadState fileName
    printState initialState
    printfn "WAITING FOR CHANGES"
    listenForChanges fileName <| onChange initialState fileName
    while (not (Console.ReadKey().KeyChar = 'q')) do 
        ignore

[<EntryPoint>]
let main argv =
    let file = Array.tryHead argv
    let a = (match file with 
        | Some fileName -> monitorFile fileName
        | None  -> (printfn "Expected a file name as arugment")
      )
    0 // return an integer exit code
