namespace Battle

open System
open System.Windows
open FSharpx.Functional.Prelude
open FsXaml
open Sodium

type DocumentView = XAML<"DocumentUserControl.xaml", true>

type DocumentViewController(container : UIElement, elements : Element seq) = 
    inherit UserControlViewController<DocumentView>()

    override __.OnLoaded view =
        let createMouseEvt e = ()
        for element in elements do
            view.Canvas.Children.Add(element.polygon) |> ignore
            element.polygon.MouseDown.Subscribe (fun )
        ()