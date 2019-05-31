// Learn more about F# at http://fsharp.org

open System
open TTSJson


[<EntryPoint>]
let main argv =
    stdin.ReadToEnd()
        |> TTSJson.load 
        |> TerraformingMars.interpret
        |> TerraformingMars.format
        |> printfn "%s"
    0

