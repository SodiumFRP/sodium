module Sodium.Frp.Time

open System
open System.Linq
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

type ITimer =
    inherit IDisposable
    abstract member Cancel : unit -> unit

type ITimerSystem<'T when 'T : comparison> =
    abstract member Time : Behavior<'T>
    abstract member At : Cell<'T option> -> Stream<'T>

type ITimerSystemImplementation<'T> =
    abstract member Start : (exn -> unit) -> unit
    abstract member SetTimer : 'T -> (unit -> unit) -> ITimer
    abstract member RunTimersTo : 'T -> unit
    abstract member Now : 'T

type private Event<'T> = { Time : 'T; Alarm : StreamSink<'T> }

type TimerSystem<'T when 'T : comparison> (implementation : 'T ITimerSystemImplementation, handleException : exn -> unit) =
    let eventQueue = Queue<Event<'T>>()
    let time = (fun () ->
        implementation.Start handleException
        let timeSink = BehaviorSink.create implementation.Now
        Transaction.onStart (fun () ->
            let t = implementation.Now
            implementation.RunTimersTo t
            let rec processEvents () =
                let event = lock eventQueue (fun () ->
                    if eventQueue.Count > 0 then
                        let event = eventQueue.Peek()
                        if event.Time <= t then
                            let event = eventQueue.Dequeue()
                            Some event
                        else None
                    else None)
                event |> Option.iter (fun event ->
                    timeSink |> BehaviorSink.send event.Time
                    event.Alarm |> StreamSink.send event.Time
                    processEvents ())
            processEvents ()
            timeSink |> BehaviorSink.send t)
        timeSink :> 'T Behavior) ()

    interface 'T ITimerSystem with
        member __.Time = time
        member __.At t =
            let alarm = StreamSink.create ()
            let mutable currentTimer : ITimer option = None
            let listener = t |> Cell.listen (fun o ->
                currentTimer |> Option.iter (fun timer -> timer.Cancel ())
                currentTimer <-
                    o |>
                        Option.map (fun time ->
                            implementation.SetTimer time (fun () ->
                                lock eventQueue (fun () -> eventQueue.Enqueue { Time = time; Alarm = alarm })
                                Transaction.run id)))
            alarm |> Stream.attachListener listener

type private WaitOrFire =
    | Wait of TimeSpan
    | Fire of (unit -> unit)

[<AbstractClass>]
type TimerSystemImplementationImplementationBase<'T when 'T : comparison>() as this =
    let lockObject = obj ()
    let timers = SortedSet<SimpleTimer<'T>> ()
    let cancellationTokenSourceLock = obj ()
    let mutable cancellationTokenSource = None
    let mutable nextSeq = 0

    let rec timeUntilNext now =
        let waitOrFire = lock lockObject (fun () ->
            if timers.Count < 1 then Wait (TimeSpan.FromSeconds (1000.0))
            else
                let timer = timers.First ()
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
                                    let cts = new CancellationTokenSource ()
                                    lock cancellationTokenSourceLock (fun () -> cancellationTokenSource <- Some cts)
                                    do! Task.Delay(waitTime, cts.Token) |> Async.AwaitTask
                                with
                                    | :? OperationCanceledException -> ()
                            finally
                                lock cancellationTokenSourceLock (fun () -> cancellationTokenSource <- None)
                    with
                        | e -> handleException e
                    return! loop ()
                }

            async { do! loop () } |> Async.Start

        member this.SetTimer time callback =
            let timer = new SimpleTimer<_> (this, time, callback)
            lock lockObject (fun () ->
                timers.Add(timer) |> ignore
                lock cancellationTokenSourceLock (fun () ->
                    cancellationTokenSource |> Option.iter (fun cts -> cts.Cancel ())))
            upcast timer

        member __.RunTimersTo now = timeUntilNext now |> ignore

        member this.Now = this.Now

and SimpleTimer<'T when 'T : comparison> (implementation : 'T TimerSystemImplementationImplementationBase, time : 'T, callback : unit -> unit) as this =
    let seq = lock implementation.LockObject (fun () ->
        let seq = implementation.NextSeq
        implementation.NextSeq <- implementation.NextSeq + 1
        seq)

    let compareEntries (x : 'T SimpleTimer) (y : 'T SimpleTimer) =
        let timeComparison = compare x.Time y.Time
        if timeComparison <> 0 then timeComparison
        else compare x.Seq y.Seq
    
    let cancel () = lock implementation.LockObject (fun () -> implementation.Timers.Remove(this) |> ignore)

    member internal __.Seq = seq
    member internal __.Time = time
    member internal __.Callback = callback

    interface ITimer with
        member this.Cancel () = cancel ()

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
        member this.Dispose () = cancel ()

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