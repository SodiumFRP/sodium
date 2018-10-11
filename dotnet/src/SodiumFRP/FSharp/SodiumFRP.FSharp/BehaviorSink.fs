module SodiumFRP.BehaviorSink

let create initialValue = BehaviorInternal.CreateSinkImpl initialValue
let createWithCoalesce initialValue coalesce = BehaviorInternal.CreateSinkImpl (initialValue, coalesce)
let send a (behaviorSink : BehaviorSink<'T>) = behaviorSink.SendImpl a