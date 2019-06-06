namespace Fridgets

open System.Windows
open System.Windows.Media
open Sodium.Frp

type FrTranslate =
    private {
        reify : Cell<Size option> -> Stream<MouseEvent> -> Stream<KeyEvent> -> Cell<int64> -> Supply.T -> Output
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
            let sMouse = sMouse |> snapshotC offset offsetMouseEvent

            let fo = Fridget.reify fridget size sMouse sKey focus idSupply

            let offsetDrawable drawable (offset : Point) =
                DrawableDelegate (fun d ->
                    d.PushTransform(TranslateTransform(offset.X, offset.Y))
                    drawable |> DrawableDelegate.invoke d
                    d.Pop())

            {
                drawable = (fo.drawable, offset) |> lift2C offsetDrawable
                desiredSize = fo.desiredSize
                sChangeFocus = fo.sChangeFocus
            }
        
        {
            reify = reify
        }