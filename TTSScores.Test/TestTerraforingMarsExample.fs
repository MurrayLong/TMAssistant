namespace Test
open Table
open TerraformingMars


#if DOTNET
open NUnit.Framework
#else
open Fable.Core.Testing
#endif

[<TestFixture>]
module TestTerraforingMarsExample =
    let gspi snapPoints (x:GameObject) = 
        let snapThreshold = 0.01f;
        let squareDistaneToSnap (snap:SnapPoint) = Vector.horizontalDistaneSquare x.Transform.Translation snap 
        let onSnap snap = (squareDistaneToSnap snap) < snapThreshold
        Option.bind (fun v->v) snapPoints

    [<Test>]
    let ``Test Example State``() = 
        let gameState = TerraformingMars.loadFile "../../../Examples/TerraformingMarsExample.json"
        gameState.O2 |> equal (Some 0)
        gameState.Temp |> equal (Some 8)
        gameState.Oceans |> equal 1
        gameState.Players |> Map.map (fun k v -> v.TR)
                          |> equal (Map.ofArray [| 
                                            (Yellow, 99); 
                                            (Green, 100); 
                                            (Red,   100); 
                                            (Blue,  100); 
                                            (Black, 100)
                                           |])

    [<Test>]
    let ``What's under debug selector`` () = 
        let scene = TTSJson.loadScene "../../../Examples/TerraformingMarsExample.json"
        let marker = findbyID "db13d7" scene 
        let snap = onSnapPoint marker  scene 
        snap |> printfn "Snap Point %A"

        //95-81
