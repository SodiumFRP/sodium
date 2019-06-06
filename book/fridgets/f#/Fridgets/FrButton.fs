namespace Fridgets

open System.Windows
open System.Windows.Input
open System.Windows.Media
open Sodium.Frp

type FrButton =
    private {
        reify : Cell<Size option> -> Stream<MouseEvent> -> Stream<KeyEvent> -> Cell<int64> -> Supply.T -> Output
        sClicked : Stream<unit>
    }
    interface IFridget with
        member this.Reify size sMouse sKey focus idSupply = this.reify size sMouse sKey focus idSupply

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FrButton =
    let create label =
        let sClicked = sinkC <| neverS ()
        let reify size sMouse _ _ _ =

            let getPressed (e : MouseEvent) size =
                match size with
                    | None -> None
                    | Some (size : Size) ->
                        match e.args with
                            | :? MouseButtonEventArgs as b ->
                                let p = e.getPosition ()
                                if b.ChangedButton = MouseButton.Left && b.ButtonState = MouseButtonState.Pressed && p.X >= 2.0 && p.X < size.Width - 2.0 && p.Y >= 2.0 && p.Y < size.Height - 2.0
                                then Some ()
                                else None
                            | _ -> None
            let sPressed = sMouse |> snapshotC size getPressed |> filterOptionS

            let getReleased (e : MouseEvent) size =
                match size with
                    | None -> None
                    | Some (_ : Size) ->
                        match e.args with
                            | :? MouseButtonEventArgs as b ->
                                if b.ChangedButton = MouseButton.Left && b.ButtonState = MouseButtonState.Released
                                then Some ()
                                else None
                            | _ -> None
            let sReleased = sMouse |> snapshotC size getReleased |> filterOptionS

            let pressed = (sPressed |> mapToS true, sReleased |> mapToS false) |> orElseS |> holdS false
            sClicked |> sendC (sReleased |> gateC pressed)

            let typeface = Typeface(FontFamily("Helvetica"), FontStyles.Normal, FontWeights.Normal, FontStretches.Normal)
            let getDesiredSize label =
                let labelSize = FontUtilities.measureString label typeface 13.0
                Size(labelSize.Width + 14.0, labelSize.Height + 10.0)
            let desiredSize = label |> mapC getDesiredSize

            let getDrawable label size pressed =
                DrawableDelegate (fun d ->
                    match size with
                        | None -> ()
                        | Some (size : Size) ->
                            d.DrawRectangle((if pressed then Brushes.DarkGray else Brushes.LightGray), Pen(Brushes.Black, 1.0), Rect(Point(2.0, 2.0), Size(size.Width - 5.0, size.Height - 5.0)))
                            let t = FontUtilities.getStandardFormattedText label typeface 13.0 Brushes.Black
                            d.DrawText(t, Point((size.Width - t.Width) / 2.0, (size.Height - t.Height) / 2.0)))

            {
                drawable = (label, size, pressed) |> lift3C getDrawable
                desiredSize = desiredSize
                sChangeFocus = neverS ()
            }
        
        {
            reify = reify
            sClicked = sClicked |> switchS
        }

    let sClicked b = b.sClicked