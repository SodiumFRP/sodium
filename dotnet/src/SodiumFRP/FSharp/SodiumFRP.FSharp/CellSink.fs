module SodiumFRP.CellSink

open System

let create initialValue = CellInternal.CreateSinkImpl initialValue
let createWithCoalesce initialValue coalesce = CellInternal.CreateSinkImpl (initialValue, Func<_,_,_> coalesce)
let send a (cellSink : CellSink<'T>) = cellSink.SendImpl a