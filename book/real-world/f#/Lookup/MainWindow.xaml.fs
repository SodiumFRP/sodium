namespace Button

open System.IO
open System.Net.Sockets
open System.Text
open System.Windows
open FsXaml
open Sodium.Frp
open SWidgets

type internal IsBusy<'T, 'TResult> (action : Stream<'T> -> Stream<'TResult>, sIn) =
    let sOut = action sIn
    let busy = (sIn |> mapToS true, sOut |> mapToS false) |> orElseS |> holdS false
    
    member __.SOut = sOut
    member __.Busy = busy

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow =
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let lookup sWord =
                let sDefinition = sinkS ()
                let listener = sWord |> listenS (fun wrd ->
                    async {
                        let mutable def = None
                        try
                            try
                                let s = new Socket (AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
                                s.Connect ("dict.org", 2628)
                                use ns = new NetworkStream (s)
                                use r = new StreamReader (ns)
                                use w = new StreamWriter (ns)
                                try
                                    r.ReadLine () |> ignore
                                    w.WriteLine (sprintf "DEFINE ! %s" wrd)
                                    w.Flush ()
                                    let mutable result = r.ReadLine ()
                                    if not (isNull result) && result.StartsWith "150" then result <- r.ReadLine ()
                                    if not (isNull result) && result.StartsWith "151" then
                                        let b = StringBuilder ()
                                        let rec readToEnd () =
                                            let l = r.ReadLine ()
                                            if l <> "." then
                                                b.AppendLine l |> ignore
                                                readToEnd ()
                                        readToEnd ()
                                        def <- Some (string b)
                                    else MessageBox.Show (sprintf "ERROR: %s" result) |> ignore
                                finally
                                    try
                                        s.Close ()
                                        s.Dispose ()
                                    with | _ -> ()
                            with | e -> MessageBox.Show (sprintf "ERROR: %A" e) |> ignore
                        finally sDefinition |> sendS def
                    } |> Async.Start)
                sDefinition |> attachListenerS listener
            let word = new STextBox ""
            let struct (enabled, (button, output)) = loopC (fun enabled ->
                let button = new SButton (enabled)
                button.Content <- "look up"
                let sWord = button.SClicked |> snapshotAndTakeC word.Text
                let ib = IsBusy<_, _> (lookup, sWord)
                let sDefinition = ib.SOut |> mapS (function Some v -> v | None -> "ERROR!")
                let definition = sDefinition |> holdS ""
                let output = (definition, ib.Busy) |> lift2C (fun definition busy -> if busy then "Looking up..." else definition)
                struct (ib.Busy |> mapC not, (button, output)))
            let outputArea = new STextBox (output |> valuesC, "", enabled)
            outputArea.TextWrapping <- TextWrapping.Wrap
            outputArea.AcceptsReturn <- true
            this.TextBoxPlaceholder.Child <- word
            this.ButtonPlaceholder.Child <- button
            this.OutputPlaceholder.Child <- outputArea