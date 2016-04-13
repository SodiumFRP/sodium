namespace Fridgets

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open Sodium

type FrView(window : Window, fr : IFridget, l : IListener) as this =
    inherit Canvas()

    let init () =
        let sMouse = Stream.sink ()
        let sKey = Stream.sink ()
        let getMouseEvent args = { args = args; getPosition = (fun () -> args.GetPosition(this)) }
        this.MouseDown.Subscribe (getMouseEvent >> sMouse.Send) |> ignore
        this.MouseUp.Subscribe (getMouseEvent >> sMouse.Send) |> ignore
        this.MouseMove.Subscribe (getMouseEvent >> sMouse.Send) |> ignore
        let size = Cell.sink Option.None
        let getNewSize (args : SizeChangedEventArgs) = Option.Some args.NewSize
        this.SizeChanged.Subscribe (getNewSize >> size.Send) |> ignore
        let onKeyDown (args : KeyEventArgs) =
            let key = if args.Key = Key.System then args.SystemKey else args.Key
            if key = Key.Back then sKey.Send BackspaceKeyEvent
        window.KeyDown.Subscribe onKeyDown |> ignore
        let getStringKeyEvent (args : TextCompositionEventArgs) = StringKeyEvent args.Text
        window.TextInput.Subscribe (getStringKeyEvent >> sKey.Send) |> ignore
        let (_, fo) = Cell.loop (fun focus ->
            let fo = Fridget.reify fr size sMouse sKey focus (Supply.create ())
            (fo.sChangeFocus |> Stream.hold -1L, fo))
        let drawable = fo.drawable
        let l = Listener.fromSeq [ l; drawable |> Operational.updates |> Stream.listen (fun _ -> this.InvalidateVisual()) ]
        (l, drawable)
    let (l, drawable) = init ()

    override __.OnRender(dc) =
        base.OnRender(dc)
        drawable |> Cell.sample |> DrawableDelegate.invoke dc

    interface IDisposable with
        member __.Dispose() = l.Unlisten ()