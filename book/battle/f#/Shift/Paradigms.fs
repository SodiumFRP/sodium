namespace Shift

open System
open System.Collections.Concurrent
open System.Threading
open System.Windows.Threading
open System.Windows.Controls
open Shift.DoubleExtensionMethods
open Sodium

type IParadigm =
    inherit IDisposable
    abstract member HandleMouseDown : MouseEvtWithElement -> unit
    abstract member HandleMouseMove : MouseEvt -> unit
    abstract member HandleMouseUp : MouseEvt -> unit
    abstract member HandleShift : bool -> unit

type private Evt =
    | EvtDown of me : MouseEvtWithElement
    | EvtMove of me : MouseEvt
    | EvtUp of me : MouseEvt
    | ShiftEvt of isDown : bool

type Actor(addMessage : string -> unit, dispatcher : Dispatcher) =
    let cts = new CancellationTokenSource()
    let ``in`` = new BlockingCollection<Evt>(1)

    do
        let out = new BlockingCollection<Reposition>(1)

        async {
            let rec loop () =
                async {
                    let! token = Async.CancellationToken

                    let mutable axisLock = false

                    let rec dragLoop () =
                        token.ThrowIfCancellationRequested()
                        let me = ``in``.Take(token)
                        match me with
                            | EvtDown me -> dispatcher.Invoke(fun () ->
                                {
                                    me = me
                                    originalLeft = Double.zeroIfNaN (Canvas.GetLeft(me.element.polygon))
                                    originalTop = Double.zeroIfNaN (Canvas.GetTop(me.element.polygon))
                                })
                            | EvtMove _ | EvtUp _ -> dragLoop ()
                            | ShiftEvt isDown ->
                                axisLock <- isDown
                                dragLoop ()

                    let rec draggingLoop dragInfo =
                        token.ThrowIfCancellationRequested()
                        let me = ``in``.Take(token)
                        match me with
                        | EvtUp _ -> ()
                        | EvtDown _ -> draggingLoop dragInfo
                        | EvtMove me ->
                            out.Add(Reposition(dragInfo, me, axisLock), token)
                            draggingLoop dragInfo
                        | ShiftEvt isDown ->
                            axisLock <- isDown
                            draggingLoop dragInfo

                    token.ThrowIfCancellationRequested()
                    let dragInfo = dragLoop ()
                    dispatcher.Invoke(fun () -> addMessage (sprintf "actor dragging %s" dragInfo.me.element.name))
                    draggingLoop dragInfo
                    do! loop ()
                }

            try
                do! loop ()
            with
                | :? OperationCanceledException -> ()
        } |> Async.Start
        
        async {
            let rec loop () =
                async {
                    let! token = Async.CancellationToken
                    token.ThrowIfCancellationRequested()
                    let r = out.Take(token)
                    dispatcher.Invoke(fun () ->
                        Canvas.SetLeft(r.Polygon, r.Left)
                        Canvas.SetTop(r.Polygon, r.Top))
                    do! loop ()
                }

            try
                do! loop ()
            with
                | :? OperationCanceledException -> ()
        } |> Async.Start

    interface IParadigm with
        member __.HandleMouseDown me = async { ``in``.Add(EvtDown me, cts.Token) } |> Async.Start
        member __.HandleMouseMove me = async { ``in``.Add(EvtMove me, cts.Token) } |> Async.Start
        member __.HandleMouseUp me = async { ``in``.Add(EvtUp me, cts.Token) } |> Async.Start
        member __.HandleShift isDown = async { ``in``.Add(ShiftEvt isDown, cts.Token) } |> Async.Start

    interface IDisposable with
        member __.Dispose() = cts.Cancel()
        
type Classic(addMessage : string -> unit) =
    let mutable dragInfo = Option<DragInfo>.None
    let mutable axisLock = false

    interface IParadigm with
        member __.HandleMouseDown me =
            addMessage (sprintf "classic dragging %s" me.element.name)
            dragInfo <- Option.Some
                {
                    me = me
                    originalLeft = Double.zeroIfNaN (Canvas.GetLeft(me.element.polygon))
                    originalTop = Double.zeroIfNaN (Canvas.GetTop(me.element.polygon))
                }

        member __.HandleMouseMove me =
            match dragInfo with
                | None -> ()
                | Some dragInfo ->
                    let r = Reposition(dragInfo, me, axisLock)
                    Canvas.SetLeft(r.Polygon, r.Left)
                    Canvas.SetTop(r.Polygon, r.Top)

        member __.HandleMouseUp _ = dragInfo <- Option.None

        member __.HandleShift isDown = axisLock <- isDown

    interface IDisposable with
        member __.Dispose() = ()
        
type Frp(addMessage : string -> unit) =
    let sMouseDown = Stream.sink ()
    let sMouseMove = Stream.sink ()
    let sMouseUp = Stream.sink ()
    let sShift = Stream.sink ()

    let listener = Transaction.Run (fun () ->
        let dragInfo = sMouseDown |> Stream.map (fun me ->
            Option.Some { me = me; originalLeft = Double.zeroIfNaN (Canvas.GetLeft(me.element.polygon)); originalTop = Double.zeroIfNaN (Canvas.GetTop(me.element.polygon)) }) |>
                        Stream.orElse (sMouseUp |> Stream.mapTo Option.None) |> Stream.hold Option.None
        let axisLock = sShift |> Stream.hold false
        let getMouseMoveIfDragging = function
            | None -> Stream.never ()
            | Some _ -> sMouseMove :> Stream<_>
        let mouseMoveWhileDragging = dragInfo |> Cell.map getMouseMoveIfDragging |> Cell.switchS
        let listener1 = dragInfo |> Operational.value |> Stream.filterOption |> Stream.listen (fun d -> addMessage (sprintf "FRP dragging %s" d.me.element.name))
        let getRepositionIfDragging me od a =
            match od with
                | None -> Option.None
                | Some d -> Option.Some (Reposition(d, me, a))
        let listener2 = mouseMoveWhileDragging |> Stream.snapshot2 getRepositionIfDragging dragInfo axisLock |> Stream.filterOption |>
                        Stream.listen (fun p ->
                            Canvas.SetLeft(p.Polygon, p.Left)
                            Canvas.SetTop(p.Polygon, p.Top))
        Listener.fromSeq [ listener1; listener2 ])

    interface IParadigm with
        member __.HandleMouseDown me = sMouseDown.Send me
        member __.HandleMouseMove me = sMouseMove.Send me
        member __.HandleMouseUp me = sMouseUp.Send me
        member __.HandleShift me = sShift.Send me

    interface IDisposable with
        member __.Dispose() = listener.Unlisten()