namespace SodiumFRP.FSharp

[<AutoOpen>]
module Prelude =

    let inline fstS struct (x, _) = x
    let inline sndS struct (_, y) = y