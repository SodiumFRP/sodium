namespace SodiumFRP.FSharp

type CellStreamSink<'T> internal () =
    inherit StreamSink<'T> (fun left right -> right)

module CellStreamSink =
    let create<'a> () = CellStreamSink<'a> ()