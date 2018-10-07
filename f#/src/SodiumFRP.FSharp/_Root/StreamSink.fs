namespace SodiumFRP.FSharp

type StreamSink<'T> internal (coalesce) as this =
    inherit Stream<'T>()
    
    let coalescer = Stream.createCoalesceHandler coalesce this
    
    internal new () = StreamSink<'T>(fun _ _ -> invalidOp "Send was called more than once in a transaction, which isn't allowed.  To combine the streams, pass a coalescing function to the StreamSink constructor.")
    
    member internal __.SendToSink a =
        Transaction.Apply
            (fun transaction _ ->
                if TransactionInternal.inCallback > 0 then invalidOp "Send may not be called inside a Sodium callback."
                transaction.Send (fun transaction -> coalescer transaction a))
            true

module StreamSink =
    let create<'a> () = StreamSink<'a> ()
    let createWithCoalesce coalesce = StreamSink (coalesce)
    let send a (streamSink : StreamSink<'T>) = streamSink.SendToSink a