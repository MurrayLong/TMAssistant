namespace Test

[<AutoOpen>]
module Util =
    #if FABLE
    // This must be before everything else
    Fable.Import.Node.require.Invoke("babel-polyfill") |> ignore
    open Fable.Core.Testing
    #else
    open NUnit.Framework
    #endif

    // Convenience method
    let equal (expected: 'T) (actual: 'T) =
        Assert.AreEqual(expected,  actual)

    let notEqual (expected: 'T) (actual: 'T) =
        Assert.AreEqual(true, expected <> actual)