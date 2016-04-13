namespace Bounce

open System.Windows
open System.Windows.Media
open FsXaml
open Shared
open Sodium
open Sodium.Time

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() =
    inherit WindowViewController<MainView>()

    let restitution = 0.95

    let bounceAt (sys : float ITimerSystem) vel pos target =
        let getSignal t v =
            {
                t0 = t
                a = v.a
                b = v.b
                c = -(Signal.valueAt v t) * restitution
            }
        sys.At (pos |> Cell.map (fun p -> Signal.``when`` p target)) |> Stream.snapshot getSignal vel

    override __.OnLoaded view =
        let animate = Animate(AnimationDelegate (fun sys extents ->
            let time = sys.Time
            let t0 = time |> Cell.sample
            let ballRadius = 15.0
            let leftWall = -extents.X + ballRadius
            let rightWall = extents.X - ballRadius
            let floor = -extents.Y + ballRadius
            let roof = extents.Y - ballRadius
            let gravity = { t0 = t0; a = 0.0; b = 0.0; c = -1200.0 }
            let (_, (posx, posy)) = Stream.loop (fun sBounceX ->
                let (_, (posx, posy, velx)) = Stream.loop (fun sBounceY ->
                    let velx = sBounceX |> Stream.hold { t0 = t0; a = 0.0; b = 0.0; c = 350.0 }
                    let vely = sBounceY |> Stream.hold (Signal.integrate gravity 0.0)
                    let posx = Signal.integrateCell velx leftWall
                    let posy = Signal.integrateCell vely roof
                    (bounceAt sys vely posy floor, (posx, posy, velx)))
                (bounceAt sys velx posx leftWall |> Stream.orElse (bounceAt sys velx posx rightWall), (posx, posy)))
            Shapes.translate (Shapes.scale (Shapes.circle Colors.Red) (Cell.constant ballRadius)) (Cell.lift3 (fun t x y -> Point(Signal.valueAt x t, Signal.valueAt y t)) time posx posy)), view.Placeholder.RenderSize)
        view.Placeholder.Children.Add(animate) |> ignore
        animate.Start ()