module Sodium.Frp.CellSink

open System
open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let create initialValue = CellInternal.CreateSinkImpl initialValue

[<MethodImpl(MethodImplOptions.NoInlining)>]
let createWithCoalesce initialValue coalesce = CellInternal.CreateSinkImpl (initialValue, Func<_,_,_> coalesce)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let send a (cellSink : CellSink<'T>) = cellSink.SendImpl a