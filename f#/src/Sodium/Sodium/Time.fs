module Sodium.Time

open System
open System.Linq
open System.Collections.Generic
open Sodium
open System.Threading
open System.Threading.Tasks

type ITimer =
    inherit IDisposable
    abstract member Cancel : unit -> unit

type ITimerSystem<'T when 'T : comparison> =
    abstract member Time : 'T Cell
    abstract member At : 'T option Cell -> 'T Stream

type 'T ITimerSystemImplementation =
    abstract member Start : (exn -> unit) -> unit
    abstract member SetTimer : 'T -> (unit -> unit) -> ITimer
    abstract member RunTimersTo : 'T -> unit
    abstract member Now : 'T

type private 'T Event = { time : 'T; alarm : 'T StreamSink }

[<AllowNullLiteral>]
type private 'T QueuedEvent(ev : 'T Event) =
    member __.Event = ev

type TimerSystem<'T when 'T : comparison> (implementation : 'T ITimerSystemImplementation, handleException : exn -> unit) =
    let eventQueue = Queue<'T QueuedEvent>()
    let time = Cell.loopWithNoCaptures (fun _ ->
        implementation.Start handleException
        let timeSink = Cell.sink implementation.Now
        Transaction.OnStart (fun () ->
            let t = implementation.Now
            implementation.RunTimersTo t
            let rec processEvents () =
                let ev = lock eventQueue (fun () ->
                    if eventQueue.Count > 0 then
                        let tempEvent = eventQueue.Peek()
                        if tempEvent <> null && tempEvent.Event.time <= t then
                            let evLocal = eventQueue.Dequeue()
                            if evLocal <> null then Option.Some evLocal.Event else Option.None
                        else Option.None
                     else Option.None)
                match ev with
                    | None -> ()
                    | Some ev ->
                        timeSink.Send ev.time
                        ev.alarm.Send ev.time
                        processEvents ()
            processEvents ()
            timeSink.Send t)
        timeSink :> 'T Cell)

    interface 'T ITimerSystem with
        member __.Time = time
        member __.At t =
            let alarm = Stream.sink ()
            let mutable currentTimer = Option<ITimer>.None
            let l = t |> Cell.listen (fun o ->
                match currentTimer with
                    | None -> ()
                    | Some timer -> timer.Cancel ()
                currentTimer <-
                    match o with
                        | None -> Option.None
                        | Some time -> Option.Some (implementation.SetTimer time (fun () ->
                            lock eventQueue (fun () -> eventQueue.Enqueue (QueuedEvent<'T>({ time = time; alarm = alarm })))
                            Transaction.Run id)))
            alarm |> Stream.addCleanup l

[<AbstractClass>]
type TimerSystemImplementationImplementationBase<'T when 'T : comparison>() as this =
    let lockObject = obj()
    let timers = SortedSet<'T SimpleTimer>()
    let cancellationTokenSourceLock = obj()
    let mutable cancellationTokenSource = Option<CancellationTokenSource>.None
    let mutable nextSeq = 0

    let rec timeUntilNext now =
        let waitOrFire = lock lockObject (fun () ->
            if timers.Count < 1 then Wait (TimeSpan.FromSeconds(1000.0))
            else
                let timer = timers.First()
                let waitTime = this.SubtractTimes timer.Time now
                if waitTime <= TimeSpan.Zero then
                    timers.Remove(timer) |> ignore
                    Fire timer.Callback
                else Wait waitTime)
        match waitOrFire with
            | Wait waitTime -> waitTime
            | Fire callback ->
                callback()
                timeUntilNext now

    member internal __.LockObject = lockObject
    member internal __.Timers = timers
    member val internal NextSeq = nextSeq with get, set

    abstract member SubtractTimes : 'T -> 'T -> TimeSpan

    abstract member Now : 'T

    interface 'T ITimerSystemImplementation with
        member this.Start handleException =
            let rec loop () =
                async {
                    try
                        let waitTime = timeUntilNext this.Now
                        if waitTime > TimeSpan.Zero then
                            try
                                try
                                    let cts = new CancellationTokenSource()
                                    lock cancellationTokenSourceLock (fun () -> cancellationTokenSource <- Option.Some cts)
                                    do! Task.Delay(waitTime, cts.Token) |> Async.AwaitIAsyncResult |> Async.Ignore
                                with
                                    | :? OperationCanceledException -> ()
                            finally
                                lock cancellationTokenSourceLock (fun () -> cancellationTokenSource <- Option.None)
                    with
                        | e -> handleException e
                    do! loop ()
                }

            async {
                do! loop ()
            } |> Async.Start

        member this.SetTimer time callback =
            let timer = new SimpleTimer<'T>(this, time, callback)
            lock lockObject (fun () ->
                timers.Add(timer) |> ignore
                lock cancellationTokenSourceLock (fun () ->
                    match cancellationTokenSource with
                        | None -> ()
                        | Some cts -> cts.Cancel()))
            timer :> ITimer

        member __.RunTimersTo now = timeUntilNext now |> ignore

        member this.Now = this.Now

and private WaitOrFire =
    | Wait of TimeSpan
    | Fire of (unit -> unit)

and SimpleTimer<'T when 'T : comparison>(implementation : 'T TimerSystemImplementationImplementationBase, time : 'T, callback : unit -> unit) =
    let seq = lock implementation.LockObject (fun () ->
        let seq = implementation.NextSeq
        implementation.NextSeq <- implementation.NextSeq + 1
        seq)

    let compareEntries (x : 'T SimpleTimer) (y : 'T SimpleTimer) =
        let timeComparison = compare x.Time y.Time
        if timeComparison <> 0 then timeComparison
        else compare x.Seq y.Seq

    member internal __.Seq = seq
    member internal __.Time = time
    member internal __.Callback = callback

    interface ITimer with
        member this.Cancel () = lock implementation.LockObject (fun () ->
            implementation.Timers.Remove(this) |> ignore)

    override this.Equals(otherObj) =
        match otherObj with
        | :? SimpleTimer<'T> as other -> this.Time = other.Time && this.Seq = other.Seq
        | _ -> false

    override this.GetHashCode() = hash this.Time

    interface IComparable<'T SimpleTimer> with
        member this.CompareTo other = compareEntries this other

    interface IComparable with
        member this.CompareTo otherObj =
            match otherObj with
            | :? SimpleTimer<'T> as other -> compareEntries this other
            | _ -> invalidArg "other" "Cannot compare values of different types."

    interface IDisposable with
        member __.Dispose() = ()

type private SystemClockTimerSystemImplementation() =
    inherit TimerSystemImplementationImplementationBase<DateTime>()
    override __.SubtractTimes first second = first - second
    override __.Now = DateTime.Now

type SystemClockTimerSystem(handleException : exn -> unit) =
    inherit TimerSystem<DateTime>(SystemClockTimerSystemImplementation(), handleException)
    
type private SecondsTimerSystemImplementation() =
    inherit TimerSystemImplementationImplementationBase<float>()
    let startTime = DateTime.Now
    override __.SubtractTimes first second = TimeSpan.FromSeconds(first - second)
    override __.Now = (DateTime.Now - startTime).TotalSeconds

type SecondsTimerSystem(handleException : exn -> unit) =
    inherit TimerSystem<float>(SecondsTimerSystemImplementation(), handleException)