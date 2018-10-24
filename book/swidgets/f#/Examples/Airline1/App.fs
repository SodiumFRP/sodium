open System
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =
    let mainWindow = Airline1.MainWindow ()
    mainWindow |> App().Run