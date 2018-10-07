namespace SodiumFRP.FSharp

type CellSink<'T> =
    inherit Cell<'T>
    
    val private cellStreamSink : CellStreamSink<'T>
    
    internal new (initialValue) =
        let cellStreamSink = CellStreamSink()
        { inherit Cell<_>(Behavior<_>(cellStreamSink, initialValue)); cellStreamSink = cellStreamSink }
    
    member internal this.Send a = this.cellStreamSink |> StreamSink.send a

module CellSink =
    let create initialValue = CellSink initialValue
    let createWithCoalesce initialValue coalesce = CellSink (initialValue, coalesce)
    let send a (cellSink : CellSink<'T>) = cellSink.Send a