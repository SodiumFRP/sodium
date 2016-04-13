namespace ClearField

open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let clear = new SButton(Content = "Clear", Width = 75.0)
        let sClearIt = clear.SClicked |> Stream.mapTo ""
        let text = new STextBox(sClearIt, "Hello", Width = 100.0)

        view.Container.Children.Add(text) |> ignore
        view.Container.Children.Add(clear) |> ignore