namespace Fwoomph

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
            let maxSize = 200.0
            let getScale t =
                let frac = t - floor t
                (if frac < 0.5 then frac else 1.0 - frac) * maxSize
            Shapes.scale (Shapes.circle Colors.Green) (time |> Cell.map getScale)
            ), view.Placeholder.RenderSize)
        view.Placeholder.Children.Add(animate) |> ignore
        animate.Start ()