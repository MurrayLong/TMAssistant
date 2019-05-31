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
module TestTransform =
    #if DOTNET
    #else
    // This must be before everything else
    Fable.Import.Node.require.Invoke("babel-polyfill") |> ignore
    #endif


    [<Test>]
    let ``Test apply transform`` ()=
        let transform = { unitCube with Translation=(20.0f,0.0f,0.0f) }
        transform.Apply Vector.origin |> equal (20.0f, 0.0f, 0.0f)

        (*
    [<Test>]
    let ``Test CoordinateSystem example``() = 
        let exampleScene = TTSJson.loadScene "examples/CoordinateSystemTest.json"
        let cube = exampleScene.Hands.HandTransforms.[0].Transform
        let cornerCubes = exampleScene.ObjectStates.[1..] |> Array.map (fun v->v.Transform.Position)
        printf "%A" cornerCubes
        cube.Corners |> equal cornerCubes

    [<Test>]
    let ``Test  More complexity``() = 
        let exampleScene = TTSJson.loadScene "examples/TestTransform.json"
        let cube = exampleScene.Hands.HandTransforms.[0].Transform
        let cornerCubes = exampleScene.ObjectStates.[1..] |> Array.map (fun v->v.Transform.Position)
        printf "%A" cornerCubes
        cube.Corners |> equal cornerCubes



    [<Test>]
    let ``Test reverse``()=
        let transform = { posX = 10.0f; posY = 5.0f; posZ = 5.0f;
                          rotX = 0.0f; rotY = 0.0f; rotZ = 0.0f;
                          scaleX = 3.0f; scaleY = 10.0f; scaleZ = 1.0f;}

        transform.Corners |> Array.map transform.Reverse |> equal unitCube.Corners
        *)

