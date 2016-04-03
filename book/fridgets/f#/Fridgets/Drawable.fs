namespace Fridgets

open System.Windows.Media

type DrawableDelegate = DrawableDelegate of (DrawingContext -> unit)
    with
        static member invoke (DrawableDelegate d) drawingContext = d drawingContext
        static member append (DrawableDelegate first) (DrawableDelegate second) =
            DrawableDelegate (fun d ->
                first d
                second d)
        member this.Invoke drawingContext = DrawableDelegate.invoke this drawingContext