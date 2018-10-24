open System
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =
    Flow.MainWindow () |> App().Run
