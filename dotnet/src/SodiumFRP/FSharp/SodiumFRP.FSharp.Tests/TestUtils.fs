namespace SodiumFRP.Tests

[<AutoOpen>]
module internal Utils =
    open NUnit.Framework
    
    let inline flip f x y = f y x

    let assertExists notExistsMessage onExists = function
    | Some o -> onExists o
    | None -> Assert.Fail notExistsMessage
    
    let assertExceptionExists onExists (e : #exn option) =
        assertExists "No exception was encountered." onExists e