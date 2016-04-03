namespace Fridgets

open System.Windows
open System.Windows.Controls
open Sodium

type FrFlow =
    private {
        reify : Size option Cell -> MouseEvent Stream -> KeyEvent Stream -> int64 Cell -> Supply.T -> Output
    }
    interface IFridget with
        member this.Reify size sMouse sKey focus idSupply = this.reify size sMouse sKey focus idSupply

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FrFlow =
    let create orientation fridgets =
        let reify size sMouse sKey focus idSupply =
            
            let foldFridget (output, idSupply) fridget =
                let (_, (fo, idSupply)) = Cell.loop (fun childSize ->
                    let getOffset () =
                        match orientation with
                            | Orientation.Horizontal -> output.desiredSize |> Cell.map (fun desiredSize -> Point(desiredSize.Width, 0.0))
                            | Orientation.Vertical -> output.desiredSize |> Cell.map (fun desiredSize -> Point(0.0, desiredSize.Height))
                            | _ -> invalidArg "orientation" "Unexpected value for Orientation"
                    let translatedFridget = FrTranslate.create fridget (getOffset ())
                    let fo = Fridget.reify translatedFridget childSize sMouse sKey focus (Supply.child1 idSupply)
                    let idSupply = Supply.child2 idSupply
                    let getChildSize oSize (desiredSize : Size) =
                        match oSize with
                            | None -> Option.None
                            | Some (size : Size) ->
                                Option.Some
                                    (match orientation with
                                        | Orientation.Horizontal -> Size(desiredSize.Width, size.Height)
                                        | Orientation.Vertical -> Size(size.Width, desiredSize.Height)
                                        | _ -> invalidArg "orientation" "Unexpected value for Orientation")
                    (Cell.lift2 getChildSize size fo.desiredSize, (fo, idSupply)))
                let getDesiredSize (ds1 : Size) (ds2 : Size) =
                    match orientation with
                        | Orientation.Horizontal -> Size(ds1.Width + ds2.Width, max ds1.Height ds2.Height)
                        | Orientation.Vertical -> Size(max ds1.Width ds2.Width, ds1.Height + ds2.Height)
                        | _ -> invalidArg "orientation" "Unexpected value for Orientation"
                ({
                    drawable = Cell.lift2 DrawableDelegate.append output.drawable fo.drawable
                    desiredSize = Cell.lift2 getDesiredSize output.desiredSize fo.desiredSize
                    sChangeFocus = output.sChangeFocus |> Stream.orElse fo.sChangeFocus
                }, idSupply)

            let initialOutput =
                {
                    drawable = Cell.constant (DrawableDelegate (fun _ -> ()))
                    desiredSize = Cell.constant (Size(0.0, 0.0))
                    sChangeFocus = Stream.never ()
                }
            fridgets |> List.fold foldFridget (initialOutput, idSupply) |> fst
        
        {
            reify = reify
        }