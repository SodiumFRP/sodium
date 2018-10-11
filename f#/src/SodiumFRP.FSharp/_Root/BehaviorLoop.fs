namespace SodiumFRP.FSharp

type internal ValuePropertyOrLazyInitialValueOrNoValue<'T> = | ValueProperty of 'T | LazyInitialValue of Lazy<'T> | NoValue

type BehaviorLoop<'T> =
    inherit Behavior<'T>
    
    val private streamLoop : StreamLoop<'T>
    val private setLazyInitialValue : Lazy<'T> -> unit
    
    internal new () =
        let mutable valuePropertyOrLazyInitialValue = NoValue
        let setValueProperty value = valuePropertyOrLazyInitialValue <- ValueProperty value
        let sampleNoTransaction () =
            match valuePropertyOrLazyInitialValue with
                | ValueProperty value -> value
                | LazyInitialValue l ->
                    let value = l.Value
                    valuePropertyOrLazyInitialValue <- ValueProperty <| value
                    value
                | NoValue -> invalidOp "BehaviorLoop was sampled before it was looped."
        let streamLoop = StreamLoop()
        let setLazyInitialValue lazyInitialValue = valuePropertyOrLazyInitialValue <- LazyInitialValue lazyInitialValue
        { inherit Behavior<_>(streamLoop, setValueProperty, sampleNoTransaction); streamLoop = streamLoop; setLazyInitialValue = setLazyInitialValue }
    
    member internal this.Loop transaction (behavior : Behavior<'T>) =
        this.streamLoop.Loop transaction <| behavior.Updates ()
        this.setLazyInitialValue <| behavior.SampleLazy transaction