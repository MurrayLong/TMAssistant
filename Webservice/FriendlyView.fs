module FriendlyView
open Giraffe
open TerraformingMars
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks
open FSharp.Data

module View =
    open GiraffeViewEngine
    let private playerName = function
        | Green -> "Murray"
        | Red -> "Sam"
        | Yellow -> "Ross"
        | _-> "Not Playing"

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  []
            ]
            body [] content
        ]

    let globalRow name value = 
        tr [] [
            td [] []
            td [] [ encodedText name]
            td [] [ encodedText value]
        ]

    let globalTable (model:GameState) = 
        table [] [
            globalRow "Generation" (model.Generation
                                    |> Option.map (sprintf "%i")
                                    |> Option.defaultValue "")
                                
            globalRow "Start Player" <| playerName model.StartPlayer
            globalRow "Next Player" <| playerName model.CurrentPlayer
            globalRow "Temp" (model.Temp 
                                |> Option.map (sprintf "%i°")
                                |> Option.defaultValue "")

            globalRow "Oceans placed" <| sprintf "%i/9" model.Oceans

            globalRow "O₂" (model.O2 
                                |> Option.map (sprintf "%i")
                                |> Option.defaultValue "")

        ]

    let headers titles = 
        tr [] (List.map (fun title -> th [] [ encodedText  title]) titles)

    
    let playerRow (player, state) =
        ((playerName player) :: (
            Output.ScoreBot.playerProperties
                |> List.map (fun (_,f) -> f state)
                |> List.map (Option.map (sprintf "%i"))
                |> List.map (Option.defaultValue "")
            )
        ) 
            |> List.map encodedText
            |> List.map (fun v-> td [] [v])
            |> tr []


    let playerTable players  = 
        table [ _id  "playerTable"; _border "1"] 
            ((headers <| "" :: List.map fst Output.ScoreBot.playerProperties )
             :: (players |> List.take 3 |> List.map playerRow))
            

    let index (model : GameState) =
        [
            h1 [] [ 
                encodedText "We are waiting for " 
                encodedText <| playerName model.CurrentPlayer 
                ]
            globalTable model
            playerTable (Map.toList model.Players)
        ]  
        |> layout


let fileUrl = "https://4hkknq.ch.files.1drv.com/y4mXyKEaokarDRTasKnN956qseS7s-sStex1tjW6koC1mc02UeT2pUlkIxeEwfovIThE4aa1iZxu7FqfEJYODBwCHI8cYrDywwZw56CoI6xGTAKxfGMbnXVeKY0-BiJqczo8garIh6RC-N7XjzBU70KR8JTKj1LgvJjbT94ucHl5S2jWPtY0r6vP_szc02FpZw3ekOEX4dlWv70Lpboc7jodw"

let handler : HttpHandler  =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task { 
            let! file = Http.AsyncRequestString(fileUrl) 
                        |> Async.StartAsTask
            let state = TerraformingMars.interpret <| TTSJson.load file
            let view      = View.index state
            return! htmlView view next ctx
        } 

