namespace Fridgets

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open Sodium.Frp

type FrView(window : Window, fr : IFridget, l : IListener) as this =
    inherit Canvas()

    let init () =
        let flip f x y = f y x
        let sMouse = sinkS ()
        let sKey = sinkS ()
        let getMouseEvent args = { args = args; getPosition = (fun () -> args.GetPosition(this)) }
        this.MouseDown.Subscribe (getMouseEvent >> (sMouse |> flip sendS)) |> ignore
        this.MouseUp.Subscribe (getMouseEvent >> (sMouse |> flip sendS)) |> ignore
        this.MouseMove.Subscribe (getMouseEvent >> (sMouse |> flip sendS)) |> ignore
        let size = sinkC None
        let getNewSize (args : SizeChangedEventArgs) = Some args.NewSize
        this.SizeChanged.Subscribe (getNewSize >> (size |> flip sendC)) |> ignore
        let onKeyDown (args : KeyEventArgs) =
            let key = if args.Key = Key.System then args.SystemKey else args.Key
            if key = Key.Back then sKey |> sendS BackspaceKeyEvent
        window.KeyDown.Subscribe onKeyDown |> ignore
        let getStringKeyEvent (args : TextCompositionEventArgs) = StringKeyEvent args.Text
        window.TextInput.Subscribe (getStringKeyEvent >> (sKey |> flip sendS)) |> ignore
        let struct (_, fo) = loopC (fun focus ->
            let fo = Fridget.reify fr size sMouse sKey focus (Supply.create ())
            struct (fo.sChangeFocus |> holdS -1L, fo))
        let drawable = fo.drawable
        let l = Listener.fromSeq [ l; upcast (drawable |> updatesC |> listenS (fun _ -> this.InvalidateVisual())) ]
        (l, drawable)
    let (l, drawable) = init ()

    override __.OnRender(dc) =
        base.OnRender(dc)
        drawable |> sampleC |> DrawableDelegate.invoke dc

    interface IDisposable with
        member __.Dispose() = l.Unlisten ()