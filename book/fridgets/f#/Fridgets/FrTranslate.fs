namespace Fridgets

open System.Windows
open System.Windows.Media
open Sodium

type FrTranslate =
    private {
        reify : Size option Cell -> MouseEvent Stream -> KeyEvent Stream -> int64 Cell -> Supply.T -> Output
    }
    interface IFridget with
        member this.Reify size sMouse sKey focus idSupply = this.reify size sMouse sKey focus idSupply

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FrTranslate =
    let create fridget offset =
        let reify size sMouse sKey focus idSupply =
            
            let offsetMouseEvent sMouse (offset : Point) =
                let getPosition () =
                    let p = sMouse.getPosition ()
                    Point(p.X - offset.X, p.Y - offset.Y)
                { args = sMouse.args; getPosition = getPosition }
            let sMouse = sMouse |> Stream.snapshot offsetMouseEvent offset

            let fo = Fridget.reify fridget size sMouse sKey focus idSupply

            let offsetDrawable drawable (offset : Point) =
                DrawableDelegate (fun d ->
                    d.PushTransform(TranslateTransform(offset.X, offset.Y))
                    drawable |> DrawableDelegate.invoke d
                    d.Pop())

            {
                drawable = Cell.lift2 offsetDrawable fo.drawable offset
                desiredSize = fo.desiredSize
                sChangeFocus = fo.sChangeFocus
            }
        
        {
            reify = reify
        }