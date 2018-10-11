module SodiumFRP.CellStreamSink

open System

let create<'a> () = CellInternal.CreateStreamSinkImpl<'a> ()
let createWithCoalesce coalesce = CellInternal.CreateStreamSinkImpl (Func<_,_,_> coalesce)