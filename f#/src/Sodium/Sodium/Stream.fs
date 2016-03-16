namespace Sodium

module Stream =
    open System.Threading.Tasks

    let sink () = new StreamSink<_>()

    let sinkWithCoalesce coalesce = new StreamSink<_>(coalesce)

    let send a (streamSink : 'T StreamSink) = streamSink.Send a

    let loop (f : 'T Stream -> ('T Stream * 'a)) =
        Transaction.Run (fun () ->
            let l = new StreamLoop<'T>()
            let (s, r) = f l
            l.Loop(s)
            (s, r))

    let loopWithNoCaptures (f : 'T Stream -> 'T Stream) =
        let (l, _) = loop (fun s -> (f s, ()))
        l

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
        stream |> listen (fun a ->
            match listenerReference with
            | None -> ()
            | Some l -> l.Unlisten()
            tcs.TrySetResult(a) |> ignore) |> ignore
        Async.AwaitTask tcs.Task

    let map (f : 'T -> 'a) (stream : 'T Stream) =
        let out = new Stream<_>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a))
        out.AddCleanup listener

    let hold initialValue (stream : 'T Stream) =
        Transaction.Apply (fun _ -> new Cell<'T>(stream, initialValue))
    
    let internal holdLazyInternal (_ : Transaction) initialValue (stream : 'T Stream) =
        new LazyCell<'T>(stream, Option.Some initialValue) :> 'T Cell

    let holdLazy initialValue (stream : 'T Stream) =
        Transaction.Apply (fun transaction -> holdLazyInternal transaction initialValue stream)
    
    let snapshot f (cell : 'T1 Cell) (stream : 'T Stream) =
        let out = new Stream<'a>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a (cell.SampleNoTransaction ())))
        out.AddCleanup listener

    let snapshotAndTakeCell cell stream = snapshot (fun _ b -> b) cell stream

    let snapshot2 f (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (stream : 'T Stream) =
        let out = new Stream<'a>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ())))
        out.AddCleanup listener

    let snapshot3 f (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (cell3 : 'T3 Cell) (stream : 'T Stream) =
        let out = new Stream<'a>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ()) (cell3.SampleNoTransaction ())))
        out.AddCleanup listener

    let snapshot4 f (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (cell3 : 'T3 Cell) (cell4 : 'T4 Cell) (stream : 'T Stream) =
        let out = new Stream<'a>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ()) (cell3.SampleNoTransaction ()) (cell4.SampleNoTransaction ())))
        out.AddCleanup listener

    let snapshot5 f (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (cell3 : 'T3 Cell) (cell4 : 'T4 Cell) (cell5 : 'T5 Cell) (stream : 'T Stream) =
        let out = new Stream<'a>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ()) (cell3.SampleNoTransaction ()) (cell4.SampleNoTransaction ()) (cell5.SampleNoTransaction ())))
        out.AddCleanup listener

    let snapshot6 f (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (cell3 : 'T3 Cell) (cell4 : 'T4 Cell) (cell5 : 'T5 Cell) (cell6 : 'T6 Cell) (stream : 'T Stream) =
        let out = new Stream<'a>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ()) (cell3.SampleNoTransaction ()) (cell4.SampleNoTransaction ()) (cell5.SampleNoTransaction ()) (cell6.SampleNoTransaction ())))
        out.AddCleanup listener

    let snapshot7 f (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (cell3 : 'T3 Cell) (cell4 : 'T4 Cell) (cell5 : 'T5 Cell) (cell6 : 'T6 Cell) (cell7 : 'T7 Cell) (stream : 'T Stream) =
        let out = new Stream<'a>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ()) (cell3.SampleNoTransaction ()) (cell4.SampleNoTransaction ()) (cell5.SampleNoTransaction ()) (cell6.SampleNoTransaction ()) (cell7.SampleNoTransaction ())))
        out.AddCleanup listener

    let snapshot8 f (cell1 : 'T1 Cell) (cell2 : 'T2 Cell) (cell3 : 'T3 Cell) (cell4 : 'T4 Cell) (cell5 : 'T5 Cell) (cell6 : 'T6 Cell) (cell7 : 'T7 Cell) (cell8 : 'T8 Cell) (stream : 'T Stream) =
        let out = new Stream<'a>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            out.Send(transaction, f a (cell1.SampleNoTransaction ()) (cell2.SampleNoTransaction ()) (cell3.SampleNoTransaction ()) (cell4.SampleNoTransaction ()) (cell5.SampleNoTransaction ()) (cell6.SampleNoTransaction ()) (cell7.SampleNoTransaction ()) (cell8.SampleNoTransaction ())))
        out.AddCleanup listener

    let private mergeInternal (other : 'T Stream) (stream : 'T Stream) =
        let out = new Stream<'T>()
        let left = Node<'T>(0L)
        let right = out.Node
        let _, nodeTarget = left.Link (fun _ _ -> ()) right
        let h t a = out.Send(t, a)
        let listener1 = stream.ListenWithTransaction left h
        let listener2 = other.ListenWithTransaction right h
        ((out.AddCleanup listener1).AddCleanup listener2).AddCleanup (Listener.fromAction(fun () -> left.Unlink nodeTarget))

    let merge (f : 'T -> 'T -> 'T) (other : 'T Stream) (stream : 'T Stream) =
        Transaction.Apply (fun transaction ->
            (mergeInternal other stream).Coalesce transaction f)
    
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
        out.AddCleanup listener

    let filterOption (stream : 'T option Stream) =
        let out = new Stream<'T>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction a ->
            match a with
            | None -> ()
            | Some v -> out.Send(transaction, v))
        out.AddCleanup listener

    let gate (cell : bool Cell) (stream : 'T Stream) =
        snapshot (fun a predicate -> if predicate then Option.Some a else Option.None) cell stream |> filterOption
    
    let collectLazy (f : 'T -> 'TState -> 'a * 'TState) (initialState : Lazy<'TState>) (stream : 'T Stream) =
        loop (fun stateLoop ->
            let s = stateLoop |> holdLazy initialState
            let both = stream |> snapshot f s
            (both |> map (fun x -> snd x), both |> map (fun x -> fst x))) |> snd
    
    let collect (f : 'T -> 'TState -> 'a * 'TState) (initialState : 'TState) (stream : 'T Stream) =
        collectLazy f (lazy initialState) stream
        
    let internal calmInternal (initialValue : Lazy<'T option>) (stream : 'T Stream when 'T : equality) =
        collectLazy (fun a lastA ->
            if (match lastA with | None -> false | Some v -> v = a)
            then (Option.None, lastA)
            else
                let oa = Option.Some a
                (oa, oa)) initialValue stream |> filterOption

    let calm (stream : 'T Stream when 'T : equality) =
        calmInternal (lazy Option.None) stream

    let accumLazy (f: 'T -> 'a -> 'a) (initialState : Lazy<'a>) (stream : 'T Stream) =
        let result = loopWithNoCaptures (fun stateLoop ->
            let s = stateLoop |> holdLazy initialState
            stream |> snapshot f s)
        result |> holdLazy initialState

    let accum (f: 'T -> 'a -> 'a) (initialState : 'a) (stream : 'T Stream) =
        accumLazy f (lazy initialState) stream

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
        out.AddCleanup listener