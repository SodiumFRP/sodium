namespace Cross

open System.Windows
open System.Windows.Media
open FsXaml
open Shared
open Sodium

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() =
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let animate = Animate(AnimationDelegate (fun sys _ ->
            let time = sys.Time
            let maxSize = 105.0
            let getOffset t =
                let frac = t - floor t
                (if frac < 0.5 then frac - 0.25 else 0.75 - frac) * 4.0 * maxSize
            let offset = time |> Cell.map getOffset
            let fifty = Cell.constant 50.0
            let onXaxis x = Point(x, 0.0)
            let onYaxis y = Point(0.0, y)
            let greenBall = Shapes.translate (Shapes.scale (Shapes.circle Colors.Green) fifty) (offset |> Cell.map onXaxis)
            let blueBall = Shapes.translate (Shapes.scale (Shapes.circle Colors.Blue) fifty) (offset |> Cell.map onYaxis)
            Shapes.over greenBall blueBall), view.Placeholder.RenderSize)
        view.Placeholder.Children.Add(animate) |> ignore
        animate.Start ()