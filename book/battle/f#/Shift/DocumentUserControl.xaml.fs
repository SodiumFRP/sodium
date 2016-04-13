namespace Shift

open System.Windows
open FsXaml

type DocumentView = XAML<"DocumentUserControl.xaml", true>

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DocumentView =
    open System.Windows.Input

    let init (d : DocumentView) (window : Window) (container : UIElement) (elements : Element seq) (paradigm : IParadigm) =
        let getKey (e : KeyEventArgs) = if e.Key = Key.System then e.SystemKey else e.Key

        for element in elements do
            d.Canvas.Children.Add(element.polygon) |> ignore
            element.polygon.MouseDown.Subscribe (fun args -> paradigm.HandleMouseDown { element = element; pt = (args.GetPosition(d)) }) |> ignore
            container.MouseMove.Subscribe (fun args -> paradigm.HandleMouseMove { pt = (args.GetPosition(d)) }) |> ignore
            container.MouseUp.Subscribe (fun args -> paradigm.HandleMouseUp { pt = (args.GetPosition(d)) }) |> ignore

            let mutable isLeftDown = false
            let mutable isRightDown = false

            window.KeyDown.Subscribe (fun args ->
                let key = getKey args
                if key = Key.LeftShift then isLeftDown <- true
                if key = Key.RightShift then isRightDown <- true
                paradigm.HandleShift (isLeftDown || isRightDown)) |> ignore

            window.KeyUp.Subscribe (fun args ->
                let key = getKey args
                if key = Key.LeftShift then isLeftDown <- false
                if key = Key.RightShift then isRightDown <- false
                paradigm.HandleShift (isLeftDown || isRightDown)) |> ignore