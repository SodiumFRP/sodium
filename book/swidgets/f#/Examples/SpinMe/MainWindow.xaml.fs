namespace SpinMe

open FsXaml
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let spinner = SSpinner.create 0
        spinner.Width <- 75.0
        view.Container.Children.Add(spinner) |> ignore