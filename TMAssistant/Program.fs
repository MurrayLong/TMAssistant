// Learn more about F# at http://fsharp.org
open System
open Argu
open FSharp.Configuration
open System.IO

// Let the type provider do it's work
type Config = YamlConfig<"config.yaml">

type GenerationSubCommand =
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Null_Sub
with
    interface IArgParserTemplate with
        member this.Usage = "required"

[<RequireSubcommand>]
type CLIArguments =
    | [<CliPrefix(CliPrefix.None)>] Generation of ParseResults<GenerationSubCommand>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Generation _ -> "Run generation"

let diff state1 state2 : (string*string*string) seq = 
    let kv1 = Output.KeyValuePairs.keyValuePairs state1 
                |> Map.ofList
    let kv2 = Output.KeyValuePairs.keyValuePairs state2
    seq { 
        for (key,value) in kv2 do
            let other = Map.tryFind key kv1
            match other with
                | None ->() 
                | Some x when x = value -> ()
                | Some x -> yield key,x,value
        done
    }

let describeDiff d = 
    let lines =  Seq.map (fun (value,expected,actual)-> sprintf "%s: %s -> %s" value expected actual) d
    String.Join ("\r\n", lines)

let watchFile path callback = 
    let watcher = new System.IO.FileSystemWatcher(path);
    watcher.EnableRaisingEvents <-true
    watcher.Changed.Add(fun _->callback())
    ()

let onFileUpdate expectedState file = 
    let currentState = File.ReadAllText(file) 
                        |> TTSJson.load 
                        |> TerraformingMars.interpret
    let remainingChanges = diff currentState expectedState  |> List.ofSeq
    printfn "UPDATE DETECTED"
    if List.isEmpty remainingChanges then
        printfn "ALL DONE!"
    else
        printfn "OUTSTANDING CHANGES" 
        printfn "%s" (describeDiff remainingChanges)

let generation (config:Config) = 
    printfn "Loading %s" config.SaveFile
    printfn "Running generation"
    let currentState = File.ReadAllText(config.SaveFile) 
                        |> TTSJson.load 
                        |> TerraformingMars.interpret
    let expectedState = Actions.generation currentState
    printfn "%s" (Output.HumanReadable.format currentState)
    printfn "%s" (describeDiff (diff currentState expectedState))
    watchFile config.SaveFile (fun ()->onFileUpdate expectedState config.SaveFile)


let run config = function
    | Generation _-> generation config

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    try
        let config=new Config()
        config.Load(@"config.yaml")
        let parser = ArgumentParser.Create<CLIArguments>(programName = "TMAssistant.exe")
        let results = parser.Parse argv
        let subCommand = results.TryGetSubCommand()
        if (subCommand.IsSome) then
            run config subCommand.Value 
        else
            printf "Invalid command \r\n%s" (parser.PrintUsage())

    with e ->
        printfn "%s" e.Message
    0
