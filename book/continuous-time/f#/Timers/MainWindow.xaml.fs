namespace Timers

open System
open System.Windows.Controls
open FsXaml
open Sodium.Frp
open Sodium.Frp.Time

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() =
    inherit MainWindowBase()

    override this.OnLoaded (_, _) =
        async {
            let rethrowOnDispatcher e = this.Dispatcher.Invoke (fun () -> raise e)
            let sys = SystemClockTimerSystem(rethrowOnDispatcher) :> DateTime ITimerSystem
            let time = sys.Time
            let sMain = sinkS ()
            let addMessage message =
                let addMessage () =
                    this.StackPanel.Children.Add(new TextBlock(Text = message)) |> ignore
                    this.ScrollViewer.ScrollToBottom()
                if this.Dispatcher.CheckAccess() then addMessage () else this.Dispatcher.Invoke addMessage
            let periodic period (sys : DateTime ITimerSystem) =
                let time = sys.Time
                let struct (_, sAlarm) = loopC (fun oAlarm ->
                    let sAlarm = sys.At oAlarm
                    let getAlarm t = Some (t + period)
                    struct (sAlarm |> mapS getAlarm |> holdS (getAlarm (time |> sampleB)), sAlarm))
                sAlarm
            let l = runT (fun () ->
                let t0 = time |> sampleB
                let l1 = sys |> periodic (TimeSpan.FromSeconds(1.0)) |> listenS (fun t -> addMessage (sprintf "%A timer" (t - t0)))
                let l2 = sMain |> snapshotAndTakeB time |> listenS (fun t -> addMessage (sprintf "%A main" (t - t0)))
                Listener.fromSeq [l1;l2])
            for _i = 0 to 4 do
                sMain |> sendS ()
                do! Async.Sleep 990
            l |> unlistenL
            } |> Async.StartImmediate
        ()