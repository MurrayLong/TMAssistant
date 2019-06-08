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
        gameState.CurrentPlayer |> equal Yellow
        gameState.StartPlayer |> equal Green
        gameState.Oceans |> equal 3
        gameState.O2 |> equal (Some 2)
        gameState.Temp |> equal (Some -24)
        gameState.Players |> Map.map (fun k v -> v.TR)
                          |> equal (Map.ofArray [| 
                                            (Yellow, 21); 
                                            (Green, 30); 
                                            (Red,   22); 
                                            (Blue,  0); 
                                            (Black, 0)
                                           |])
