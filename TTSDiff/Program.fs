// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Threading
open Output.ScoreBot


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

let printDiff diff= 
    if (List.length diff = 0) then
        printfn "NONE YET"
    else
        printfn "CUMULATIVE CHANGE"
        for {name=name; value=change} in diff do
            printfn "/scores @ %s:%i" name change

let onChange initialState fileName () = 
    printfn "FILE UPDATED"
    printfn "NEW STATE:"
    let newState = loadState fileName
    printState newState
    printfn "Changes"
    printDiff <| Output.ScoreBot.getDiff initialState newState
    printfn "----------------"

let monitorFile fileName = 
    printfn "Monitoring file %s" fileName
    printfn "Initial State"
    let initialState = loadState fileName
    printState initialState
    printfn "WAITING FOR CHANGES"
    listenForChanges fileName <| onChange initialState fileName
    while (not (Console.ReadKey().KeyChar = 'q')) do 
        ignore ()

[<EntryPoint>]
let main argv =
    let file = Array.tryHead argv
    let a = (match file with 
                | Some fileName -> monitorFile fileName
                | None  -> (printfn "Expected a file name as arugment")
            )
    0 // return an integer exit code
