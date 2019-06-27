// Learn more about F# at http://fsharp.org
open System
open Argu
open Output

type OutputFormat =  
  HumanReadable | Json | KeyValue

type CLIArguments =
    | Output of OutputFormat 
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Output _ -> "Specify the output format"

let format = function 
  | Json          -> Output.Json.prettyJson
  | HumanReadable -> Output.HumanReadable.format
  | KeyValue      -> Output.KeyValuePairs.format

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create<CLIArguments>(programName = "TTSScores.exe")
  let arguments = parser.Parse argv
  let outputFormat = arguments.TryGetResult Output |> Option.defaultValue HumanReadable
  let scene = stdin.ReadToEnd() |> TTSJson.load 
  scene |> TerraformingMars.interpret
        |> format outputFormat
        |> Console.WriteLine 
  0
