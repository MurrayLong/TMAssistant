namespace Test

[<AutoOpen>]
module Util =
    #if DOTNET
    #else
    // This must be before everything else
    Fable.Import.Node.require.Invoke("babel-polyfill") |> ignore
    #endif
    #if DOTNET
    open NUnit.Framework
    #else
    open Fable.Core.Testing
    #endif

    // Convenience method
    let equal (expected: 'T) (actual: 'T) =
        Assert.AreEqual(expected,  actual)

    let notEqual (expected: 'T) (actual: 'T) =
        Assert.AreEqual(true, expected <> actual)