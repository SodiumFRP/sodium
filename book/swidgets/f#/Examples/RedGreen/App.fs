open System
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =
    let mainWindow = RedGreen.MainWindow ()
    mainWindow |> App().Run