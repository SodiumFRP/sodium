open System
open System.Threading.Tasks
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =
    let app = App()
    app.Startup.Subscribe (fun _ ->
        TaskScheduler.UnobservedTaskException.Subscribe (fun args ->
            if not args.Observed then
                System.Windows.MessageBox.Show (args.Exception.Message, "An unhandled exception occurred.") |> ignore
                raise args.Exception) |> ignore) |> ignore
    new PetrolPump.PetrolPumpWindow () |> app.Run
