namespace Fridgets

open System.Windows.Media

type DrawableDelegate = DrawableDelegate of (DrawingContext -> unit)
    with
        static member invoke drawingContext (DrawableDelegate d) = d drawingContext
        static member append (DrawableDelegate first) (DrawableDelegate second) =
            DrawableDelegate (fun d ->
                first d
                second d)
        member this.Invoke drawingContext = this |> DrawableDelegate.invoke drawingContext