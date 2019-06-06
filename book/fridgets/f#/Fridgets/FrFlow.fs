namespace Fridgets

open System.Windows
open System.Windows.Controls
open Sodium.Frp

type FrFlow =
    private {
        reify : Cell<Size option> -> Stream<MouseEvent> -> Stream<KeyEvent> -> Cell<int64> -> Supply.T -> Output
    }
    interface IFridget with
        member this.Reify size sMouse sKey focus idSupply = this.reify size sMouse sKey focus idSupply

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FrFlow =
    let create orientation fridgets =
        let reify size sMouse sKey focus idSupply =
            
            let foldFridget (output, idSupply) fridget =
                let struct (_, (fo, idSupply)) = loopC (fun childSize ->
                    let getOffset () =
                        match orientation with
                            | Orientation.Horizontal -> output.desiredSize |> mapC (fun desiredSize -> Point(desiredSize.Width, 0.0))
                            | Orientation.Vertical -> output.desiredSize |> mapC (fun desiredSize -> Point(0.0, desiredSize.Height))
                            | _ -> invalidArg "orientation" "Unexpected value for Orientation"
                    let translatedFridget = FrTranslate.create fridget (getOffset ())
                    let fo = Fridget.reify translatedFridget childSize sMouse sKey focus (Supply.child1 idSupply)
                    let idSupply = Supply.child2 idSupply
                    let getChildSize oSize (desiredSize : Size) =
                        match oSize with
                            | None -> None
                            | Some (size : Size) ->
                                Some
                                    (match orientation with
                                        | Orientation.Horizontal -> Size(desiredSize.Width, size.Height)
                                        | Orientation.Vertical -> Size(size.Width, desiredSize.Height)
                                        | _ -> invalidArg "orientation" "Unexpected value for Orientation")
                    struct ((size, fo.desiredSize) |> lift2C getChildSize, (fo, idSupply)))
                let getDesiredSize (ds1 : Size) (ds2 : Size) =
                    match orientation with
                        | Orientation.Horizontal -> Size(ds1.Width + ds2.Width, max ds1.Height ds2.Height)
                        | Orientation.Vertical -> Size(max ds1.Width ds2.Width, ds1.Height + ds2.Height)
                        | _ -> invalidArg "orientation" "Unexpected value for Orientation"
                ({
                    drawable = (output.drawable, fo.drawable) |> lift2C DrawableDelegate.append
                    desiredSize = (output.desiredSize, fo.desiredSize) |> lift2C getDesiredSize
                    sChangeFocus = (output.sChangeFocus, fo.sChangeFocus) |> orElseS
                }, idSupply)

            let initialOutput =
                {
                    drawable = constantC (DrawableDelegate (fun _ -> ()))
                    desiredSize = constantC (Size(0.0, 0.0))
                    sChangeFocus = neverS ()
                }
            fridgets |> List.fold foldFridget (initialOutput, idSupply) |> fst
        
        {
            reify = reify
        }