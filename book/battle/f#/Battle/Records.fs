namespace Battle

open System
open System.Windows
open System.Windows.Shapes

type Element = { name : string; polygon : Polygon }

type MouseEvt = { pt : Point }

type MouseEvtWithElement = { element : Element; pt : Point }

type DragInfo = { me : MouseEvtWithElement; originalLeft : float; originalTop : float }

type Reposition private (output : (Polygon * Point) Lazy) =
    new(dragInfo : DragInfo, me : MouseEvt) =
        Reposition(lazy (dragInfo.me.element.polygon, Point(dragInfo.originalLeft + me.pt.X - dragInfo.me.pt.X, dragInfo.originalTop + me.pt.Y - dragInfo.me.pt.Y)))

    member __.Polygon = fst output.Value
    member __.Left = (snd output.Value).X
    member __.Top = (snd output.Value).Y

module DoubleExtensionMethods =
    type Double
        with
            static member zeroIfNaN (d : float) = if Double.IsNaN(d) then 0.0 else d