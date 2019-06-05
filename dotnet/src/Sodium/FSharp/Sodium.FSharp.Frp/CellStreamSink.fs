module Sodium.Frp.CellStreamSink

open System
open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let create<'a> () = CellInternal.CreateStreamSinkImpl<'a> ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let createWithCoalesce coalesce = CellInternal.CreateStreamSinkImpl (Func<_,_,_> coalesce)