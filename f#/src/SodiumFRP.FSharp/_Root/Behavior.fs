namespace SodiumFRP.FSharp

type Behavior<'T> private (stream : Stream<'T>, createStreamListener, setValueProperty, sampleNoTransaction) =
    let mutable valueUpdate = None
    
    let streamListener =
        if createStreamListener then
            let updateValue (transaction : Transaction) a =
                match valueUpdate with
                    | None -> transaction.Last (fun () ->
                        valueUpdate |> Option.iter setValueProperty
                        valueUpdate <- None)
                    | _ -> ()
                valueUpdate <- Some a
            Some <| Transaction.Apply
                (fun transaction _ ->
                    stream.ListenT
                        Node.Null
                        transaction
                        updateValue
                        false)
                false
        else None
    
    static let createDefaultMethods initialValue =
        let mutable valueProperty = initialValue
        let setValueProperty value = valueProperty <- value
        let sampleNoTransaction () = valueProperty
        (setValueProperty, sampleNoTransaction)
    
    internal new (value) =
        let (setValueProperty, sampleNoTransaction) = createDefaultMethods value
        Behavior (Stream<'T>(), false, setValueProperty, sampleNoTransaction)
    
    internal new (stream : Stream<'T>, initialValue) =
        let (setValueProperty, sampleNoTransaction) = createDefaultMethods initialValue
        Behavior (stream, true, setValueProperty, sampleNoTransaction)
    
    internal new (stream : Stream<'T>, setValueProperty, sampleNoTransaction) = Behavior (stream, true, setValueProperty, sampleNoTransaction)
    
    member internal __.SampleNoTransaction () = sampleNoTransaction ()
    member internal __.KeepListenersAlive = stream.KeepListenersAlive
    
    member internal __.Updates () = stream
    
    member internal __.Sample () = Transaction.Apply (fun transaction _ -> sampleNoTransaction ()) true
    
    member internal this.SampleLazy (transaction : Transaction) =
        let mutable s = HasNoValue this
        transaction.Sample (fun () ->
            s <- HasValue <| match valueUpdate with | Some v -> v | None -> sampleNoTransaction ())
        lazy (match s with | HasValue value -> value | HasNoValue behavior -> behavior.Sample ())
    
    member internal __.StreamListener = streamListener // prevent GC

and private 'T LazySample = | HasValue of value : 'T | HasNoValue of cell : Behavior<'T>