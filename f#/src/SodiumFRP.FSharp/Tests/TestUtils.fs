namespace SodiumFRP.FSharp.Tests

[<AutoOpen>]
module Utils =
    open NUnit.Framework

    let assertExists notExistsMessage onExists = function
    | Some o -> onExists o
    | None -> Assert.Fail notExistsMessage
    
    let assertExceptionExists onExists (e : #exn option) =
        assertExists "No exception was encountered." onExists e