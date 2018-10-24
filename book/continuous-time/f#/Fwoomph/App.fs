open System
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ = 
    Fwoomph.MainWindow () |> App().Run
