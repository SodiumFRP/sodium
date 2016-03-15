namespace Sodium

module Stream =
    open System.Threading.Tasks

    let sink () = new StreamSink<_>()

    let send a (streamSink : 'T StreamSink) = streamSink.Send a

    let never () = new Stream<_>()

    let listen handler (stream : 'T Stream) = stream.Listen handler

    let listenWeak handler (stream : 'T Stream) = stream.ListenWeak handler

    let addCleanup listener (stream : 'T Stream) = stream.AddCleanup listener

    let listenOnce handler (stream : 'T Stream) =
        let mutable listenerReference = Option<IListener>.None
        let listener = stream |> listen (fun a ->
            match listenerReference with
            | None -> ()
            | Some l -> l.Unlisten()
            handler a)
        listenerReference <- Option.Some listener
        listener

    let listenOnceAsync (stream : 'T Stream) =
        let tcs = TaskCompletionSource<'T>()
        let mutable listenerReference = Option<IListener>.None
        let listener = stream |> listen (fun a ->
            match listenerReference with
            | None -> ()
            | Some l -> l.Unlisten()
            tcs.TrySetResult(a) |> ignore)
        Async.AwaitTask tcs.Task

    let map (f : 'T -> 'a) (stream : 'T Stream) =
        let out = new Stream<_>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a))
        out.UnsafeAddCleanup listener

//    let hold initialValue (stream : 'T Stream) =
//        Transaction.Apply (fun transaction -> new Cell<'T>(stream, initialValue))
    
//    let internal holdLazyInternal (transaction : Transaction) initialValue (stream : 'T Stream) =
//        new LazyCell<'T>(stream, initialValue)

//    let holdLazy initialValue (stream : 'T Stream) =
//        Transaction.Apply (fun transaction -> holdLazyInternal transaction initialValue)
    
//    let snapshot (f : 'T -> 'T1 -> 'a) (cell : 'T1 Cell) (stream : 'T Stream) =
//        let out = new Stream<'T>()
//        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
//            out.Send(transaction, f a (cell.SampleNoTransaction ())))
//        out.UnsafeAddCleanup listener
//
//    let snapshotAndTakeCell cell stream = snapshot (fun _ b -> b) cell stream
//
//    let snapshot2 (f : 'T -> 'T1 -> 'T2 -> 'a) (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (stream : 'T Stream) =
//        let out = new Stream<'T>()
//        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
//            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ())))
//        out.UnsafeAddCleanup listener
//
//    let snapshot3 (f : 'T -> 'T1 -> 'T2 -> 'T3 -> 'a) (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (cell3 : 'T3 Cell) (stream : 'T Stream) =
//        let out = new Stream<'T>()
//        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
//            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ()) (cell3.SampleNoTransaction ())))
//        out.UnsafeAddCleanup listener
//
//    let snapshot4 (f : 'T -> 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'a) (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (cell3 : 'T3 Cell) (cell4 : 'T4 Cell) (stream : 'T Stream) =
//        let out = new Stream<'T>()
//        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
//            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ()) (cell3.SampleNoTransaction ()) (cell4.SampleNoTransaction ())))
//        out.UnsafeAddCleanup listener

    let private mergeInternal (other : 'T Stream) (stream : 'T Stream) =
        let out = new Stream<'T>()
        let left = Node<'T>(0L)
        let right = out.Node
        let _, nodeTarget = left.Link (fun t v -> ()) right
        let h t a = out.Send(t, a)
        let listener1 = stream.ListenWithTransaction left h
        let listener2 = stream.ListenWithTransaction right h
        ((out.UnsafeAddCleanup listener1).UnsafeAddCleanup listener2).UnsafeAddCleanup (Listener.fromAction(fun () -> left.Unlink nodeTarget))

    let merge (f : 'T -> 'T -> 'T) (other : 'T Stream) (stream : 'T Stream) =
        Transaction.Apply (fun transaction ->
            mergeInternal other stream |> stream.Coalesce transaction f)
    
    let orElse (other : 'T Stream) (stream : 'T Stream) =
        merge (fun l _ -> l) other stream

    let rec private mergeAllInternal (f : 'T -> 'T -> 'T) (streams : 'T Stream list) start ``end`` =
        let n = ``end`` - start
        if n = 0 then new Stream<'T>()
        elif n = 1 then streams.[start]
        elif n = 2 then merge f streams.[start + 1] streams.[start]
        else
            let mid = (start + ``end``) / 2
            merge f (mergeAllInternal f streams mid ``end``) (mergeAllInternal f streams start mid)

    let mergeAll (f : 'T -> 'T -> 'T) (streams : 'T Stream seq) =
        let s = List.ofSeq streams
        mergeAllInternal f s 0 s.Length

    let orElseAll (streams : 'T Stream seq) =
        mergeAll (fun l _ -> l) streams

    let filter (predicate : 'T -> bool) (stream : 'T Stream) =
        let out = new Stream<'T>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            if predicate a then out.Send(transaction, a))
        out.UnsafeAddCleanup listener

    let filterOption (stream : 'T option Stream) =
        let out = new Stream<'T>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            match a with
            | None -> ()
            | Some v -> out.Send(transaction, v))
        out.UnsafeAddCleanup listener

//    let gate (cell : bool Cell) (stream : 'T Stream) =
//        snapshot (fun a predicate -> if predicate then Option.Some a else Option.None) |> filterOption
    
//    let collectLazy (f : 'T -> 'TState -> 'TReturn * 'TState) (initialState : Lazy<'TState>) (stream : 'T option Stream) =
//        ()
    
//    let collect (f : 'T -> 'TState -> 'TReturn * 'TState) (initialState : 'TState) (stream : 'T option Stream) =
//        ()
        
//    let internal calmInternal (initialValue : Lazy<'T option>) (stream : 'T Stream when 'T : equality) =
//        ()

//    let calm (stream : 'T Stream when 'T : equality) =
//        calmInternal (lazy Option.None)

//    let accumLazy (f: 'T -> 'TReturn -> 'TReturn) (initialState : Lazy<'TReturn>) (stream : 'T option Stream) =
//        ()

//    let accum (f: 'T -> 'TReturn -> 'TReturn) (initialState : 'TReturn) (stream : 'T option Stream) =
//        accumLazy f (lazy initialState)

    let once (stream : 'T Stream) =
        let out = new Stream<'T>()
        let mutable listenerReference = Option<IListener>.None
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            match listenerReference with
            | None -> ()
            | Some l ->
                out.Send(transaction, a)
                l.Unlisten()
                listenerReference <- Option.None)
        listenerReference <- Option.Some listener
        out.UnsafeAddCleanup listener