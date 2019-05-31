namespace Test
open Enviroment

#if DOTNET
open NUnit.Framework
#else
open Fable.Core.Testing
#endif


[<TestFixture>]
module TestFSharp =
    type Person = { name: string; age: int }
    let harry = { name="Harry"; age=12 } 
    type Person with member this.House = "Green"

    [<Test>]
    let ``Test filter`` () = 
        Array.filter (fun x->x>2) [|1; 2; 3|] |> equal [|3|]

    [<Test>]
    let ``Test clone immutable``() = 
        {harry with age=20} |> equal { name="Harry"; age=20 }

    [<Test>]
    let ``Test Array Comparison``() = 
        let expected = [|1;2;3|]
        let actual = [|1;2;3|]
        actual |> equal expected
        actual |> notEqual [|1;2|]

    [<Test>]
    let ``Test Pattern matching``() = 
        let isThree x = match x with
                            | 3 -> true
                            | _ -> false

        isThree 3 |> equal true
        isThree 2 |> equal false

