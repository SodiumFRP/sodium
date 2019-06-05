module Sodium.Frp.BehaviorSink

open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let create initialValue = BehaviorInternal.CreateSinkImpl initialValue

[<MethodImpl(MethodImplOptions.NoInlining)>]
let createWithCoalesce initialValue coalesce = BehaviorInternal.CreateSinkImpl (initialValue, coalesce)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let send a (behaviorSink : BehaviorSink<'T>) = behaviorSink.SendImpl a