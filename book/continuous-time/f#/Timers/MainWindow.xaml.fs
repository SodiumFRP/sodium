namespace Timers

open System
open System.Windows.Controls
open FsXaml
open Sodium
open Sodium.Time

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() =
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        async {
            let rethrowOnDispatcher e = view.Root.Dispatcher.Invoke (fun () -> raise e)
            let sys = SystemClockTimerSystem(rethrowOnDispatcher) :> DateTime ITimerSystem
            let time = sys.Time
            let sMain = Stream.sink ()
            let addMessage message =
                let addMessage () =
                    view.StackPanel.Children.Add(new TextBlock(Text = message)) |> ignore
                    view.ScrollViewer.ScrollToBottom()
                if view.Root.Dispatcher.CheckAccess() then addMessage () else view.Root.Dispatcher.Invoke addMessage
            let periodic period (sys : DateTime ITimerSystem) =
                let time = sys.Time
                let (_, sAlarm) = Cell.loop (fun oAlarm ->
                    let sAlarm = sys.At oAlarm
                    let getAlarm t = Option.Some (t + period)
                    (sAlarm |> Stream.map getAlarm |> Stream.hold (getAlarm (time |> Cell.sample)), sAlarm))
                sAlarm
            let l = Transaction.Run (fun () ->
                let t0 = time |> Cell.sample
                let l1 = sys |> periodic (TimeSpan.FromSeconds(1.0)) |> Stream.listen (fun t -> addMessage (sprintf "%A timer" (t - t0)))
                let l2 = sMain |> Stream.snapshotAndTakeCell time |> Stream.listen (fun t -> addMessage (sprintf "%A main" (t - t0)))
                Listener.fromSeq [l1;l2])
            for _i = 0 to 4 do
                sMain.Send ()
                do! Async.Sleep 990
            l.Unlisten ()
            } |> Async.StartImmediate
        ()