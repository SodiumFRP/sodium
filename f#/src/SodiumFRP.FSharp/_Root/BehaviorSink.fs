namespace SodiumFRP.FSharp

type BehaviorSink<'T> private (streamSink : StreamSink<'T>, initialValue) =
    inherit Behavior<'T> (streamSink, initialValue)
    
    internal new (initialValue, coalesce) = BehaviorSink(StreamSink(coalesce), initialValue)
    internal new (initialValue) = BehaviorSink(initialValue, fun left right -> right)
    
    member internal __.Send a = streamSink |> StreamSink.send a

module BehaviorSink =
    let create initialValue = BehaviorSink initialValue
    let createWithCoalesce initialValue coalesce = BehaviorSink (initialValue, coalesce)
    let send a (behaviorSink : BehaviorSink<'T>) = behaviorSink.Send a