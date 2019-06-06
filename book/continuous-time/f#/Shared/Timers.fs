namespace Shared

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open System.Windows
open System.Windows.Media
open Sodium.Frp
open Sodium.Frp.Time

type private Renderer(timerSystem : CompositionTargetSecondsTimerSystem, drawable : Behavior<DrawableDelegate>, size : Size, animation : Animate, isStarted : bool Cell) =
    do
        runT (fun() -> isStarted |> valuesC |> filterS id |> listenOnceS (fun _ -> timerSystem.AddAnimation animation) |> ignore)

    member __.Render(drawingContext : DrawingContext) =
        DrawableDelegate.invoke (drawable |> sampleB) drawingContext size.Height (Point(0.0, 0.0)) 1.0

and Animate(animation : AnimationDelegate, size : Size) as this =
    inherit UIElement()

    let isStarted = sinkC false

    let init () =
        let tcs = TaskCompletionSource<Renderer>()
        let renderer = tcs.Task
        let mutable renderingSubscription : IDisposable option = None
        renderingSubscription <- Some (CompositionTarget.Rendering.Subscribe (fun args ->
            let timerSystem = CompositionTargetSecondsTimerSystem.Create (args :?> RenderingEventArgs).RenderingTime.TotalSeconds (fun e -> this.Dispatcher.Invoke (fun () -> raise e))
            let extents = Point(size.Width / 2.0, size.Height / 2.0)
            let drawable = runT (fun () -> Shapes.translate (AnimationDelegate.invoke animation timerSystem extents) (constantB extents))
            tcs.SetResult(Renderer(timerSystem, drawable, size, this, isStarted))
            match renderingSubscription with
                | None -> ()
                | Some s ->
                    s.Dispose()
                    renderingSubscription <- None))
        renderer

    let renderer = init ()

    override __.OnRender(drawingContext : DrawingContext) =
        base.OnRender(drawingContext)
        if renderer.IsCompleted then renderer.Result.Render(drawingContext)

    member __.Start() = isStarted |> sendC true

and private CompositionTargetSecondsTimerSystemImplementation(startTime : float) =
    inherit TimerSystemImplementationImplementationBase<float>()

    let mutable now = startTime
    let nowLock = obj()

    override __.SubtractTimes first second = TimeSpan.FromSeconds(first - second)
    override __.Now = lock nowLock (fun () -> now)

    member __.SetNow now' = now <- now'

and CompositionTargetSecondsTimerSystem private (implementation : CompositionTargetSecondsTimerSystemImplementation, handleException : exn -> unit) =
    inherit TimerSystem<float>(implementation, handleException)

    let animationsToRun = ConcurrentBag<Animate>()

    do
        CompositionTarget.Rendering.Subscribe (fun args ->
            implementation.SetNow (args :?> RenderingEventArgs).RenderingTime.TotalSeconds
            for animation in animationsToRun.ToArray() do animation.InvalidateVisual()) |> ignore

    member __.AddAnimation animation = animationsToRun.Add(animation)

    static member Create startTime handleException = CompositionTargetSecondsTimerSystem(CompositionTargetSecondsTimerSystemImplementation(startTime), handleException)
    
and private CompositionTargetTimerSystemImplementation(startTime : TimeSpan) =
    inherit TimerSystemImplementationImplementationBase<TimeSpan>()

    let mutable now = startTime
    let nowLock = obj()

    override __.SubtractTimes first second = first - second
    override __.Now = lock nowLock (fun () -> now)

    member __.SetNow now' = now <- now'

and CompositionTargetTimerSystem private (implementation : CompositionTargetTimerSystemImplementation, handleException : exn -> unit) =
    inherit TimerSystem<TimeSpan>(implementation, handleException)

    let animationsToRun = ConcurrentBag<Animate>()

    do
        CompositionTarget.Rendering.Subscribe (fun args ->
            implementation.SetNow (args :?> RenderingEventArgs).RenderingTime
            for animation in animationsToRun.ToArray() do animation.InvalidateVisual()) |> ignore

    member __.AddAnimation animation = animationsToRun.Add(animation)

    static member Create startTime handleException = CompositionTargetTimerSystem(CompositionTargetTimerSystemImplementation(startTime), handleException)