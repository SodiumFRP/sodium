namespace Shift2

open System
open System.Windows
open System.Windows.Shapes

type Element = { name : string; polygon : Polygon }

type MouseEvt = { pt : Point }

type MouseEvtWithElement = { element : Element; pt : Point }

type DragInfo = { me : MouseEvtWithElement; originalLeft : float; originalTop : float }

type Reposition private (output : (Polygon * Point) Lazy) =
    new(dragInfo : DragInfo, me : MouseEvt, axisLock : bool) =
        let defaultTx = lazy (me.pt.X - dragInfo.me.pt.X)
        let defaultTy = lazy (me.pt.Y - dragInfo.me.pt.Y)

        let (tx, ty) =
            if axisLock then
                if Math.Abs(defaultTx.Value) < Math.Abs(defaultTy.Value) then (0.0, defaultTy.Value) else (defaultTx.Value, 0.0)
            else (defaultTx.Value, defaultTy.Value)

        Reposition(lazy (dragInfo.me.element.polygon, Point(dragInfo.originalLeft + tx, dragInfo.originalTop + ty)))

    member __.Polygon = fst output.Value
    member __.Left = (snd output.Value).X
    member __.Top = (snd output.Value).Y

module DoubleExtensionMethods =
    type Double
        with
            static member zeroIfNaN (d : float) = if Double.IsNaN(d) then 0.0 else d