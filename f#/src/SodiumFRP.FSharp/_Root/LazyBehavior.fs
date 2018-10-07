namespace SodiumFRP.FSharp

type internal ValuePropertyOrLazyInitialValue<'T> = | ValueProperty of 'T | LazyInitialValue of Lazy<'T>

type internal LazyBehavior<'T> =
    inherit Behavior<'T>
    
    internal new ((transaction : Transaction), stream, lazyInitialValue) =
        let mutable valuePropertyOrLazyInitialValue = LazyInitialValue lazyInitialValue
        let setValueProperty value = valuePropertyOrLazyInitialValue <- ValueProperty value
        let sampleNoTransaction () =
            match valuePropertyOrLazyInitialValue with
                | ValueProperty value -> value
                | LazyInitialValue l ->
                    let value = l.Value
                    valuePropertyOrLazyInitialValue <- ValueProperty <| value
                    value
        let ensureValueIsCreated () = sampleNoTransaction () |> ignore
        { inherit Behavior<_>(stream, setValueProperty, sampleNoTransaction) }
        then transaction.Sample ensureValueIsCreated