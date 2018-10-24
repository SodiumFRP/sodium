open System
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =
    let mainWindow = Spinner.MainWindow ()
    mainWindow |> App().Run