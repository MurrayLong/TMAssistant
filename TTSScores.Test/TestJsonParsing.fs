namespace Test
open Enviroment
open Table

#if FABLE
open Fable.Core.Testing
#else
open NUnit.Framework
#endif

[<TestFixture>]
module TestJsonParsing =
    #if FABLE
    // This must be before everything else
    Fable.Import.Node.require.Invoke("babel-polyfill") |> ignore
    #else
    #endif

    type NamedObject = {Name:string}

    [<Test>]
    let ``Test Parse Object`` ()=
        let json = """{ "Name": "Die_6_Rounded"}"""
        (ofJson<NamedObject> json).Name |>  equal "Die_6_Rounded"

    [<Test>]
    let ``Test Parse Array`` () = 
        let json = """ [ { "Name": "Die_6_Rounded" } ] """
        ofJson<NamedObject[]> json |> Array.length |> equal 1

    [<Test>]
    let ``Test Parse List`` () = 
        let json = """ [ { "Name": "Die_6_Rounded" } ] """
        ofJson<List<NamedObject>> json |> List.length |> equal 1
