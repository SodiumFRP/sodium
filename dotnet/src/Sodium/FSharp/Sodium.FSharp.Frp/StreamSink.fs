module Sodium.Frp.StreamSink

open System
open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let create<'a> () = StreamInternal.CreateSinkImpl<'a> ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let createWithCoalesce coalesce = StreamInternal.CreateSinkImpl (Func<_,_,_> coalesce)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let send a (streamSink : StreamSink<'T>) = streamSink.SendImpl a