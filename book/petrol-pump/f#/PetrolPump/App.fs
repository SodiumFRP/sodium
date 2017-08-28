open System
open System.Threading.Tasks
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =
    let app = App().Root
    app.Startup.Subscribe (fun _ ->
        TaskScheduler.UnobservedTaskException.Subscribe (fun args ->
            if not args.Observed then raise args.Exception) |> ignore) |> ignore
    app.Run()
