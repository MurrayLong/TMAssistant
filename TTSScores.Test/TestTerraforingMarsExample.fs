namespace Test
open Table
open Transform
open TerraformingMars


#if DOTNET
open NUnit.Framework
#else
open Fable.Core.Testing
#endif

[<TestFixture>]
module TestTerraforingMarsExample =
    [<Test>]
    let ``Test O2`` () = 
        let GameState = TerraformingMars.loadFile "../../../Examples/TerraformingMarsExample.json"
        TerraformingMars.format GameState |> printf "%s"

    let gspi snapPoints (x:GameObject) = 
        let snapThreshold = 0.01f;
        let squareDistaneToSnap (snap:SnapPoint) = Vector.horizontalDistaneSquare x.Transform.Translation snap 
        let onSnap snap = (squareDistaneToSnap snap) < snapThreshold
(*        Option.bind (Array.tryFindIndex onSnap) snapPoints *)
        Option.bind (fun v->v) snapPoints

    [<Test>]
    let ``Test Example State``() = 
        let gameState = TerraformingMars.loadFile "../../../Examples/TerraformingMarsExample.json"
        gameState.O2 |> equal (Some 0)
        gameState.Temp |> equal (Some 8)
        gameState.TR |> equal (Map.ofArray [| 
                                            (Yellow, Some 99); 
                                            (Green, Some 100); 
                                            (Red, Some 100); 
                                            (Blue, Some 100); 
                                            (Black, Some 100)
                                           |])

    [<Test>]
    let ``What's under debug selector`` () = 
        let scene = TTSJson.loadScene "../../../Examples/TerraformingMarsExample.json"
        let marker = findbyID scene "db13d7"
        let snap = onSnapPoint scene marker
        snap |> printfn "Snap Point %A"

        //95-81
