namespace Shared

open System.Windows
open System.Windows.Media
open Sodium.Frp
open Sodium.Frp.Time

type Height = double
type Origin = Point
type Scale = double
type DrawableDelegate = DrawableDelegate of (DrawingContext -> Height -> Origin -> Scale -> unit)
    with
        static member invoke (DrawableDelegate d) drawingContext height origin scale = d drawingContext height origin scale
        static member append (DrawableDelegate first) (DrawableDelegate second) =
            DrawableDelegate (fun d h o s ->
                first d h o s
                second d h o s)
        member this.Invoke drawingContext height origin scale = DrawableDelegate.invoke this drawingContext height origin scale

type Extents = Point
type AnimationDelegate = AnimationDelegate of (ITimerSystem<double> -> Extents -> Behavior<DrawableDelegate>)
    with
        member this.Invoke timerSystem extents = match this with AnimationDelegate d -> d timerSystem extents
        static member invoke (AnimationDelegate d) timerSystem extents = d timerSystem extents