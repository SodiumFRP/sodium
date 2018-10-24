namespace SpinMe

open FsXaml
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let spinner = SSpinner.create 0
            spinner.Width <- 75.0
            this.Container.Children.Add(spinner) |> ignore