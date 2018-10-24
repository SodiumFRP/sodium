open System
open FsXaml
open System.Windows

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =
    Bounce.MainWindow () |> App().Run