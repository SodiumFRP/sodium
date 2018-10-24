open System
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =
    let mainWindow = Label.MainWindow ()
    mainWindow |> App().Run