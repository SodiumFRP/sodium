namespace Fridgets

open System.Windows
open System.Windows.Input
open System.Windows.Media
open Sodium.Frp

type FrTextField =
    private {
        reify : Cell<Size option> -> Stream<MouseEvent> -> Stream<KeyEvent> -> Cell<int64> -> Supply.T -> Output
        text : Cell<string>
    }
    interface IFridget with
        member this.Reify size sMouse sKey focus idSupply = this.reify size sMouse sKey focus idSupply

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FrTextField =
    type private TextUpdate = { text : string; newX : int }

    let create initialText =
        let substring (s : string) startIndex length = s.Substring(startIndex,length)
        let substringToEnd (s : string) startIndex = s.Substring(startIndex)

        let textSink = sinkC (constantC "")
        let text = textSink |> switchC
        let reify size sMouse sKey focus idSupply =

            let getPressed (e : MouseEvent) size =
                match size with
                    | None -> None
                    | Some (size : Size) ->
                        match e.args with
                            | :? MouseButtonEventArgs as b ->
                                let p = e.getPosition ()
                                if b.ChangedButton = MouseButton.Left && b.ButtonState = MouseButtonState.Pressed && p.X >= 2.0 && p.X < size.Width - 2.0 && p.Y >= 2.0 && p.Y < size.Height - 2.0
                                then Some (p.X - 2.0)
                                else None
                            | _ -> None
            let sPressed = sMouse |> snapshotC size getPressed |> filterOptionS

            let myId = Supply.get idSupply
            let haveFocus = focus |> mapC ((=) myId)
            let typeface = Typeface(FontFamily("Helvetica"), FontStyles.Normal, FontWeights.Normal, FontStretches.Normal)
            let struct (x, sTextUpdate) = loopC (fun x ->
                let getTextUpdate key (text : string) =
                    let xValue = x |> sampleC
                    match key with
                        | BackspaceKeyEvent _ ->
                            if xValue > 0
                            then Some { text = (substring text 0 (xValue - 1)) + (substringToEnd text xValue); newX = xValue - 1 }
                            else None
                        | StringKeyEvent stringKey ->
                            if stringKey = "\b"
                            then None
                            else Some { text = (substring text 0 xValue) + stringKey + (substringToEnd text xValue); newX = xValue + 1 }
                let sTextUpdate = sKey |> gateC haveFocus |> snapshotC text getTextUpdate |> filterOptionS
                let getX x (text : string) =
                    let rec checkLength n =
                        if n > text.Length
                        then text.Length
                        else
                            if x < (FontUtilities.measureString (substring text 0 n) typeface 13.0).Width
                            then n - 1
                            else checkLength (n + 1)
                    checkLength 1
                struct (
                    (sPressed |> snapshotC text getX, sTextUpdate |> mapS (fun u -> u.newX)) |> orElseS |> holdS 0,
                    sTextUpdate
                ))
            textSink |> sendC (sTextUpdate |> mapS (fun u -> u.text) |> holdS initialText)

            let getDesiredSize text =
                let size = FontUtilities.measureString text typeface 13.0
                Size(size.Width + 14.0, size.Height + 10.0)
            let desiredSize = text |> mapC getDesiredSize

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
                drawable = (text, x, haveFocus, size) |> lift4C getDrawable
                desiredSize = desiredSize
                sChangeFocus = sPressed |> mapToS myId
            }
        
        {
            reify = reify
            text = text
        }

    let text (t : FrTextField) = t.text