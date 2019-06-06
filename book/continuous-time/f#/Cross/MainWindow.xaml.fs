namespace Cross

open System.Windows
open System.Windows.Media
open FsXaml
open Shared
open Sodium.Frp

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() =
    inherit MainWindowBase()

    override this.OnLoaded (_, _) =
        let animate = Animate(AnimationDelegate (fun sys _ ->
            let time = sys.Time
            let maxSize = 105.0
            let getOffset t =
                let frac = t - floor t
                (if frac < 0.5 then frac - 0.25 else 0.75 - frac) * 4.0 * maxSize
            let offset = time |> mapB getOffset
            let fifty = Cell.constant 50.0
            let onXaxis x = Point(x, 0.0)
            let onYaxis y = Point(0.0, y)
            let greenBall = Shapes.translate (Shapes.scale (Shapes.circle Colors.Green) (fifty |> asBehaviorC)) (offset |> mapB onXaxis)
            let blueBall = Shapes.translate (Shapes.scale (Shapes.circle Colors.Blue) (fifty |> asBehaviorC)) (offset |> mapB onYaxis)
            Shapes.over greenBall blueBall), this.Placeholder.RenderSize)
        this.Placeholder.Children.Add(animate) |> ignore
        animate.Start ()