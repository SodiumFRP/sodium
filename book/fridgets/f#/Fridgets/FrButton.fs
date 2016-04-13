namespace Fridgets

open System.Windows
open System.Windows.Input
open System.Windows.Media
open Sodium

type FrButton =
    private {
        reify : Size option Cell -> MouseEvent Stream -> KeyEvent Stream -> int64 Cell -> Supply.T -> Output
        sClicked : unit Stream
    }
    interface IFridget with
        member this.Reify size sMouse sKey focus idSupply = this.reify size sMouse sKey focus idSupply

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FrButton =
    let create label =
        let sClicked = Cell.sink (Stream.never ())
        let reify size sMouse _ _ _ =

            let getPressed (e : MouseEvent) size =
                match size with
                    | None -> Option.None
                    | Some (size : Size) ->
                        match e.args with
                            | :? MouseButtonEventArgs as b ->
                                let p = e.getPosition ()
                                if b.ChangedButton = MouseButton.Left && b.ButtonState = MouseButtonState.Pressed && p.X >= 2.0 && p.X < size.Width - 2.0 && p.Y >= 2.0 && p.Y < size.Height - 2.0
                                then Option.Some ()
                                else Option.None
                            | _ -> Option.None
            let sPressed = sMouse |> Stream.snapshot getPressed size |> Stream.filterOption

            let getReleased (e : MouseEvent) size =
                match size with
                    | None -> Option.None
                    | Some (_ : Size) ->
                        match e.args with
                            | :? MouseButtonEventArgs as b ->
                                if b.ChangedButton = MouseButton.Left && b.ButtonState = MouseButtonState.Released
                                then Option.Some ()
                                else Option.None
                            | _ -> Option.None
            let sReleased = sMouse |> Stream.snapshot getReleased size |> Stream.filterOption

            let pressed = sPressed |> Stream.mapTo true |> Stream.orElse (sReleased |> Stream.mapTo false) |> Stream.hold false
            sClicked.Send (sReleased |> Stream.gate pressed)

            let typeface = Typeface(FontFamily("Helvetica"), FontStyles.Normal, FontWeights.Normal, FontStretches.Normal)
            let getDesiredSize label =
                let labelSize = FontUtilities.measureString label typeface 13.0
                Size(labelSize.Width + 14.0, labelSize.Height + 10.0)
            let desiredSize = label |> Cell.map getDesiredSize

            let getDrawable label size pressed =
                DrawableDelegate (fun d ->
                    match size with
                        | None -> ()
                        | Some (size : Size) ->
                            d.DrawRectangle((if pressed then Brushes.DarkGray else Brushes.LightGray), Pen(Brushes.Black, 1.0), Rect(Point(2.0, 2.0), Size(size.Width - 5.0, size.Height - 5.0)))
                            let t = FontUtilities.getStandardFormattedText label typeface 13.0 Brushes.Black
                            d.DrawText(t, Point((size.Width - t.Width) / 2.0, (size.Height - t.Height) / 2.0)))

            {
                drawable = Cell.lift3 getDrawable label size pressed
                desiredSize = desiredSize
                sChangeFocus = Stream.never ()
            }
        
        {
            reify = reify
            sClicked = sClicked |> Cell.switchS
        }

    let sClicked b = b.sClicked