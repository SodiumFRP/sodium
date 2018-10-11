module SodiumFRP.StreamSink

open System

let create<'a> () = StreamInternal.CreateSinkImpl<'a> ()
let createWithCoalesce coalesce = StreamInternal.CreateSinkImpl (Func<_,_,_> coalesce)
let send a (streamSink : StreamSink<'T>) = streamSink.SendImpl a