namespace Sodium

open System

type private 'T LazySample =
    | HasNoValue of cell : 'T CellImpl
    | HasValue of value : 'T

and internal 'T CellImpl private (stream : 'T StreamImpl, value : 'T option, setupListener : bool) =
    let mutable value = value
    let mutable valueUpdate = Option<'T>.None

    let cleanup =
        if setupListener then
            Transaction.Apply (fun transaction ->
                stream.ListenInternal Node<'T>.Null transaction (fun transaction a ->
                    match valueUpdate with
                    | None ->
                        transaction.Last (fun () ->
                            match valueUpdate with
                            | None -> ()
                            | Some v ->
                                value <- Option.Some v
                                valueUpdate <- Option.None)
                    | Some _ -> ()
                    valueUpdate <- Option.Some a) false)
        else Listener.empty

    internal new(value : 'T) =
        new CellImpl<'T>(new StreamImpl<'T>(), Option.Some value, false)
    internal new(stream : 'T StreamImpl, initialValue : 'T) =
        new CellImpl<'T>(stream, Option.Some initialValue, true)
    internal new(stream : 'T StreamImpl) =
        new CellImpl<'T>(stream, Option.None, true)

    member val internal KeepListenersAlive = stream.KeepListenersAlive

    member internal __.Value with get () = value and set v = value <- v

    member internal this.SampleLazy (transaction : Transaction) =
        let mutable s = HasNoValue this
        transaction.Last (fun () ->
            s <- HasValue (
                match valueUpdate with
                | None -> this.SampleNoTransaction ()
                | Some v -> v))
        lazy (
            match s with
            | HasNoValue cell -> cell.Sample()
            | HasValue value -> value)

    abstract member SampleNoTransaction : unit -> 'T
    default __.SampleNoTransaction () =
        match value with
        | None -> invalidOp "Sample must be overridden if value can be None."
        | Some v -> v

    member internal this.Sample () =
        Transaction.Apply (fun _ -> this.SampleNoTransaction ())

    member internal __.Updates (_ : Transaction) = stream

    abstract member Dispose : unit -> unit
    default __.Dispose() = cleanup.Unlisten ()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

type internal 'T LazyCellImpl(stream : 'T StreamImpl, lazyInitialValue : 'T Lazy option) =
    inherit CellImpl<'T>(stream = stream)

    member val internal LazyInitialValue = lazyInitialValue with get,set

    override this.SampleNoTransaction () =
        match this.Value with
            | None ->
                match this.LazyInitialValue with
                    | None -> invalidOp "LazyInitialValue must not be None if it has not been calculated."
                    | Some l ->
                        let v = l.Value
                        this.Value <- Option.Some v
                        this.LazyInitialValue <- Option.None
                        v
            | Some v -> v

type internal 'T CellSinkImpl private(streamSink : 'T StreamSinkImpl, initialValue : 'T) =
    inherit CellImpl<'T>(streamSink, initialValue)

    internal new(initialValue : 'T) =
        new CellSinkImpl<'T>(new StreamSinkImpl<'T>(), initialValue)

    internal new(initialValue : 'T, coalesce : 'T -> 'T -> 'T) =
        new CellSinkImpl<'T>(new StreamSinkImpl<'T>(coalesce), initialValue)

    member __.Send a = streamSink.Send a

    override __.Dispose () =
        base.Dispose()
        (streamSink :> IDisposable).Dispose ()

type 'T Cell internal(impl : 'T CellImpl) =
    member val internal Impl = impl

    interface IDisposable with
        member __.Dispose() = (impl :> IDisposable).Dispose()

type 'T CellSink internal(impl : 'T CellSinkImpl) =
    inherit Cell<'T>(impl)
    member val internal Impl = impl

    member __.Send a = impl.Send a

type internal 'T CellLoopImpl private(streamLoop : 'T StreamLoopImpl) =
    inherit LazyCellImpl<'T>(streamLoop, Option.None)

    internal new() = new CellLoopImpl<'T>(new StreamLoopImpl<'T>())

    member internal this.Loop (cell : 'T CellImpl) =
        Transaction.Apply (fun transaction ->
            this.LazyInitialValue <- Option.Some (cell.SampleLazy transaction)
            streamLoop.Loop (cell.Updates transaction) |> ignore)

    override __.SampleNoTransaction () =
        if not streamLoop.IsAssigned then invalidOp "CellLoop was sampled before it was looped."
        base.SampleNoTransaction ()

    override __.Dispose () =
        base.Dispose()
        (streamLoop :> IDisposable).Dispose ()

type 'T CellLoop internal(impl : 'T CellLoopImpl) =
    inherit Cell<'T>(impl)
    member val internal Impl = impl