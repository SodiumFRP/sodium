namespace Fwoomph

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
            let maxSize = 200.0
            let getScale t =
                let frac = t - floor t
                (if frac < 0.5 then frac else 1.0 - frac) * maxSize
            Shapes.scale (Shapes.circle Colors.Green) (time |> mapB getScale)
            ), this.Placeholder.RenderSize)
        this.Placeholder.Children.Add(animate) |> ignore
        animate.Start ()