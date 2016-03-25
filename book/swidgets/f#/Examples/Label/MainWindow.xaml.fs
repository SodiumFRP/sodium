namespace Label

open System.Windows
open FsXaml
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let msg = new STextBox("Hello", Width = 150.0)
        let lbl = new SLabel(msg.Text, Width = 150.0, Margin = Thickness(5.0, 0.0, 0.0, 0.0))

        view.Container.Children.Add(msg) |> ignore
        view.Container.Children.Add(lbl) |> ignore