namespace Bounce

open System.Windows
open System.Windows.Media
open FsXaml
open Shared
open Sodium.Frp
open Sodium.Frp.Time

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() =
    inherit MainWindowBase()

    let restitution = 0.95

    let bounceAt (sys : ITimerSystem<float>) vel pos target =
        let getSignal t v =
            {
                t0 = t
                a = v.a
                b = v.b
                c = -(Signal.valueAt v t) * restitution
            }
        sys.At (pos |> mapC (fun p -> Signal.``when`` p target)) |> snapshotC vel getSignal

    override this.OnLoaded (_, _) =
        let animate = Animate(AnimationDelegate (fun sys extents ->
            let time = sys.Time
            let t0 = time |> sampleB
            let ballRadius = 15.0
            let leftWall = -extents.X + ballRadius
            let rightWall = extents.X - ballRadius
            let floor = -extents.Y + ballRadius
            let roof = extents.Y - ballRadius
            let gravity = { t0 = t0; a = 0.0; b = 0.0; c = -1200.0 }
            let struct (_, (posx, posy)) = loopS (fun sBounceX ->
                let struct (_, (posx, posy, velx)) = loopS (fun sBounceY ->
                    let velx = sBounceX |> holdS { t0 = t0; a = 0.0; b = 0.0; c = 350.0 }
                    let vely = sBounceY |> holdS (Signal.integrate gravity 0.0)
                    let posx = Signal.integrateCell velx leftWall
                    let posy = Signal.integrateCell vely roof
                    struct (bounceAt sys vely posy floor, (posx, posy, velx)))
                struct ((bounceAt sys velx posx leftWall, bounceAt sys velx posx rightWall) |> orElseS, (posx, posy)))
            Shapes.translate (Shapes.scale (Shapes.circle Colors.Red) (constantB ballRadius)) (lift3B (fun t x y -> Point(Signal.valueAt x t, Signal.valueAt y t)) (time, posx |> asBehaviorC, posy |> asBehaviorC))), this.Placeholder.RenderSize)
        this.Placeholder.Children.Add(animate) |> ignore
        animate.Start ()