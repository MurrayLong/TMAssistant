namespace Test
open Transform
open Table
open Enviroment

#if DOTNET
open NUnit.Framework
#else
open Fable.Core.Testing
#endif

[<TestFixture>]
module TestTable =
    [<Test>]
    let ``Test SnapPoints `` () = 
        let scene = TTSJson.loadScene "../../../Examples/SnapPointTest.json"
        let onSnap1 = findNick "Snapped 1" scene |> Seq.head
        let onSnap2 = findNick "Snapped 2" scene |> Seq.head
        let notSnapped = findNick "NotSnapped" scene|> Seq.head

        onSnapPoint onSnap1 scene |>  equal (Some 1)
        onSnapPoint onSnap2 scene |>  equal (Some 0)
        onSnapPoint notSnapped scene |>  equal None 

    [<Test>]
    let ``Test hands `` ()=
        let scene = TTSJson.loadScene "../../../Examples/HandExample.json"
        let cards = scene.ObjectStates
        let hand = scene.Hands.HandTransforms.[0].Transform
        let inHand = cards |> Array.filter (fun v->v.Nickname="InHand")
        let outOfHand = cards |> Array.filter (fun v->v.Nickname="notInHand")
        inHand |> Array.length |> equal 6
        outOfHand |> Array.length |> equal 4
        inHand  |> Array.filter (fun v->hand.Contains v.Transform.Translation) 
                |> equal inHand
        outOfHand  |> Array.filter (fun v->hand.Contains v.Transform.Translation) 
                |> equal Array.empty
