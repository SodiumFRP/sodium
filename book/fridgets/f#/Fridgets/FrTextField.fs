namespace Fridgets

open System.Windows
open System.Windows.Input
open System.Windows.Media
open Sodium

type FrTextField =
    private {
        reify : Size option Cell -> MouseEvent Stream -> KeyEvent Stream -> int64 Cell -> Supply.T -> Output
        text : string Cell
    }
    interface IFridget with
        member this.Reify size sMouse sKey focus idSupply = this.reify size sMouse sKey focus idSupply

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FrTextField =
    type private TextUpdate = { text : string; newX : int }

    let create initialText =
        let substring (s : string) startIndex length = s.Substring(startIndex,length)
        let substringToEnd (s : string) startIndex = s.Substring(startIndex)

        let textSink = Cell.sink (Cell.constant "")
        let text = textSink |> Cell.switchC
        let reify size sMouse sKey focus idSupply =

            let getPressed (e : MouseEvent) size =
                match size with
                    | None -> Option.None
                    | Some (size : Size) ->
                        match e.args with
                            | :? MouseButtonEventArgs as b ->
                                let p = e.getPosition ()
                                if b.ChangedButton = MouseButton.Left && b.ButtonState = MouseButtonState.Pressed && p.X >= 2.0 && p.X < size.Width - 2.0 && p.Y >= 2.0 && p.Y < size.Height - 2.0
                                then Option.Some (p.X - 2.0)
                                else Option.None
                            | _ -> Option.None
            let sPressed = sMouse |> Stream.snapshot getPressed size |> Stream.filterOption

            let myId = Supply.get idSupply
            let haveFocus = focus |> Cell.map ((=) myId)
            let typeface = Typeface(FontFamily("Helvetica"), FontStyles.Normal, FontWeights.Normal, FontStretches.Normal)
            let (x, sTextUpdate) = Cell.loop (fun x ->
                let getTextUpdate key (text : string) =
                    let xValue = x |> Cell.sample
                    match key with
                        | BackspaceKeyEvent _ ->
                            if xValue > 0
                            then Option.Some { text = (substring text 0 (xValue - 1)) + (substringToEnd text xValue); newX = xValue - 1 }
                            else Option.None
                        | StringKeyEvent stringKey ->
                            if stringKey = "\b"
                            then Option.None
                            else Option.Some { text = (substring text 0 xValue) + stringKey + (substringToEnd text xValue); newX = xValue + 1 }
                let sTextUpdate = sKey |> Stream.gate haveFocus |> Stream.snapshot getTextUpdate text |> Stream.filterOption
                let getX x (text : string) =
                    let rec checkLength n =
                        if n > text.Length
                        then text.Length
                        else
                            if x < (FontUtilities.measureString (substring text 0 n) typeface 13.0).Width
                            then n - 1
                            else checkLength (n + 1)
                    checkLength 1
                (
                    sPressed |> Stream.snapshot getX text |> Stream.orElse (sTextUpdate |> Stream.map (fun u -> u.newX)) |> Stream.hold 0,
                    sTextUpdate
                ))
            textSink.Send (sTextUpdate |> Stream.map (fun u -> u.text) |> Stream.hold initialText)

            let getDesiredSize text =
                let size = FontUtilities.measureString text typeface 13.0
                Size(size.Width + 14.0, size.Height + 10.0)
            let desiredSize = text |> Cell.map getDesiredSize

            let getDrawable text x haveFocus size =
                DrawableDelegate (fun d ->
                    match size with
                        | None -> ()
                        | Some (size : Size) ->
                            d.DrawRectangle(Brushes.White, Pen(Brushes.Black, 1.0), Rect(Point(2.0, 2.0), Size(size.Width - 5.0, size.Height - 5.0)))
                            let t = FontUtilities.getStandardFormattedText text typeface 13.0 Brushes.Black
                            let tCursor = FontUtilities.getStandardFormattedText (substring text 0 x) typeface 13.0 Brushes.Black
                            d.DrawText(t, Point(4.0, (size.Height - t.Height) / 2.0))
                            if haveFocus then
                                let cursorX = tCursor.Width
                                d.DrawLine(Pen(Brushes.Red, 1.0), Point(4.0 + cursorX, 4.0), Point(4.0 + cursorX, size.Height - 5.0))
                            )

            {
                drawable = Cell.lift4 getDrawable text x haveFocus size
                desiredSize = desiredSize
                sChangeFocus = sPressed |> Stream.mapTo myId
            }
        
        {
            reify = reify
            text = text
        }

    let text (t : FrTextField) = t.text