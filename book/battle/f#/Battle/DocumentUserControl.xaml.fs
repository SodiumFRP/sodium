namespace Battle

open System.Windows
open FsXaml

type DocumentView = XAML<"DocumentUserControl.xaml", true>

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DocumentView =
    let init (d : DocumentView) (container : UIElement) (elements : Element seq) (paradigm : IParadigm) =
        for element in elements do
            d.Canvas.Children.Add(element.polygon) |> ignore
            element.polygon.MouseDown.Subscribe (fun args -> paradigm.HandleMouseDown { element = element; pt = (args.GetPosition(d)) }) |> ignore
            container.MouseMove.Subscribe (fun args -> paradigm.HandleMouseMove { pt = (args.GetPosition(d)) }) |> ignore
            container.MouseUp.Subscribe (fun args -> paradigm.HandleMouseUp { pt = (args.GetPosition(d)) }) |> ignore