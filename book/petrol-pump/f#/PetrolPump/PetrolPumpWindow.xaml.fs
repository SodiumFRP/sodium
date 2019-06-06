namespace PetrolPump

open System
open System.Collections.Generic
open System.Media
open System.Threading
open System.Threading.Tasks
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media.Imaging
open FsXaml
open PetrolPump.Controls
open Sodium.Frp
open Chapter4.Section11
open Chapter4.Section4
open Chapter4.Section6
open Chapter4.Section7
open Chapter4.Section8
open Chapter4.Section9

type PetrolPumpWindowBase = XAML<"PetrolPumpWindow.xaml">

type private SetLcdDigitsState =
    {
        append : Grid seq -> Grid -> Grid seq
        grids : Grid seq
        complete : Grid seq -> Grid seq
    }

type SegmentType = Large | Small

type PetrolPumpWindow =
    inherit PetrolPumpWindowBase
    
    val mutable listener : IListener
    
    new() as this =
        let (getImage, setLcdDigits) =
            let assemblyName = typedefof<PetrolPumpWindow>.Assembly.FullName
            let createSegment segmentType i =
                let image = Image()
                let source = BitmapImage()
                source.BeginInit ()
                source.UriSource <- Uri(sprintf "pack://application:,,,/%s;component/images/%s%i.png" assemblyName (match segmentType with | Large -> "large" | Small -> "small") i)
                source.EndInit ()
                image.Source <- source
                image.Width <- float source.PixelWidth
                image.Height <- float source.PixelHeight
                image
            let largeSegments = Array.init 8 (fun i () -> createSegment Large i)
            let smallSegments = Array.init 8 (fun i () -> createSegment Small i)
            let layouts =
                [|
                    [|0;1;2;4;5;6|];
                    [|2;5|];
                    [|0;1;3;5;6|];
                    [|0;2;3;5;6|];
                    [|2;3;4;5|];
                    [|0;2;3;4;6|];
                    [|0;1;2;3;4;6|];
                    [|2;5;6|];
                    [|0;1;2;3;4;5;6|];
                    [|2;3;4;5;6|]
                |]
            let createImage segmentType i =
                let layout = layouts.[i]
                let grid = Grid()
                for n in layout do grid.Children.Add ((match segmentType with | Large -> largeSegments | Small -> smallSegments).[n] ()) |> ignore
                grid
            let largeNumberImages = Array.init 10 (fun i () -> createImage Large i)
            let smallNumberImages = Array.init 10 (fun i () -> createImage Small i)
            let largeDotImage () =
                let grid = Grid()
                grid.Children.Add (largeSegments.[7] ()) |> ignore
                grid
            let smallDotImage () =
                let grid = Grid()
                grid.Children.Add (smallSegments.[7] ()) |> ignore
                grid
            let largeDashImage () =
                let grid = Grid()
                grid.Children.Add (largeSegments.[3] ()) |> ignore
                grid
            let smallDashImage () =
                let grid = Grid()
                grid.Children.Add (smallSegments.[3] ()) |> ignore
                grid

            let getImage c isLarge =
                match c with
                    | '-' -> if isLarge then largeDashImage () else smallDashImage ()
                    | '0' -> Array.item 0 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '1' -> Array.item 1 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '2' -> Array.item 2 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '3' -> Array.item 3 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '4' -> Array.item 4 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '5' -> Array.item 5 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '6' -> Array.item 6 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '7' -> Array.item 7 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '8' -> Array.item 8 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '9' -> Array.item 9 (if isLarge then largeNumberImages else smallNumberImages) ()
                    | '.' -> if isLarge then largeDotImage () else smallDotImage ()
                    | _ -> invalidArg "c" "Character must be a dot, dash, or number between 0 and 9."
            
            let flip f x y = f y x
            
            let setLcdDigits (placeholder : StackPanel) text maxDigits isLarge =
                placeholder.Children.Clear()
                let defaultAppend gg g = Seq.append gg [g]
                let defaultComplete = id
                let initialState = { append = defaultAppend; grids = Seq.empty; complete = defaultComplete }
                let aggregate (s : SetLcdDigitsState) c =
                    let imageGrid = getImage c isLarge
                    if c = '.' then
                        let container = Grid()
                        container.Children.Add imageGrid |> ignore
                        let append gg g =
                            container.Children.Add g |> ignore
                            s.append gg container
                        { append = append; grids = s.grids; complete = flip Seq.append [| imageGrid |] }
                    else { append = defaultAppend; grids = s.append s.grids imageGrid; complete = defaultComplete }
                let takeMax (s : SetLcdDigitsState) = s.complete s.grids |> Seq.truncate maxDigits |> Seq.rev
                let addToPlaceholder g = placeholder.Children.Add g |> ignore
                text |> Seq.rev |> Seq.fold aggregate initialState |> takeMax |> Seq.iter addToPlaceholder
            
            (getImage, setLcdDigits)
        
        { inherit PetrolPumpWindowBase (); listener = Listener.empty }
        then
            let logic = SComboBox<IPump>([| LifeCyclePump (); AccumulatePulsesPump (); ShowDollarsPump (); ClearSalePump (); KeypadPump (); PresetAmountPump () |], (fun p -> p.GetType().FullName))
            this.LogicComboBoxPlaceholder.Children.Add logic |> ignore
            let textPrice1 = STextField "2.149"
            textPrice1.Width <- 100.0
            this.Price1Placeholder.Children.Add textPrice1 |> ignore
            let textPrice2 = STextField "2.341"
            textPrice2.Width <- 100.0
            this.Price2Placeholder.Children.Add textPrice2 |> ignore
            let textPrice3 = STextField "1.499"
            textPrice3.Width <- 100.0
            this.Price3Placeholder.Children.Add textPrice3 |> ignore
            let parseDoubleSafe s =
                let (success, n) = Double.TryParse s
                if success then n else 0.0
            let sKey = sinkS ()
            let containersByKey =
                Map.ofSeq
                    [|
                        (Key.One, this.Keypad1Button);
                        (Key.Two, this.Keypad2Button);
                        (Key.Three, this.Keypad3Button);
                        (Key.Four, this.Keypad4Button);
                        (Key.Five, this.Keypad5Button);
                        (Key.Six, this.Keypad6Button);
                        (Key.Seven, this.Keypad7Button);
                        (Key.Eight, this.Keypad8Button);
                        (Key.Nine, this.Keypad9Button);
                        (Key.Zero, this.Keypad0Button);
                        (PetrolPump.Key.Clear, this.KeypadClearButton)
                    |]
            let startAsVoidTask (a : Async<unit>) : Task = upcast (a |> Async.StartAsTask)
            for (key, container) in Map.toSeq containersByKey do
                let mouseDownHandler sender (args : MouseButtonEventArgs) =
                    if args.LeftButton = MouseButtonState.Pressed then
                        async { sKey |> sendS key } |> startAsVoidTask |> ignore
                container.MouseDown.AddHandler (MouseButtonEventHandler mouseDownHandler)
            let calibration = constantC 0.001
            let price2 = textPrice2.Text |> mapC parseDoubleSafe
            let price3 = textPrice3.Text |> mapC parseDoubleSafe
            let price1 = textPrice1.Text |> mapC parseDoubleSafe
            let csClearSale = sinkC (neverS ())
            let sClearSale = csClearSale |> switchS
            let sFuelPulses = sinkS ()
            
            let listeners = List<IListener>()
            
            let listenAndCreateNozzle nozzleLoop (nozzleImage : Image) =
                listeners.Add (nozzleLoop |> listenC (fun p -> this.Dispatcher.InvokeAsync (fun () -> nozzleImage.Margin <- match p with | Up -> Thickness(0.0, 0.0, 0.0, 0.0) | Down -> Thickness(0.0, 30.0, 0.0, 0.0)) |> ignore))
                let nozzleClicks = sinkS ()
                let mouseDownHandler sender (args : MouseButtonEventArgs) =
                    if args.LeftButton = MouseButtonState.Pressed then
                        async { nozzleClicks |> sendS () } |> startAsVoidTask |> ignore
                nozzleImage.MouseDown.AddHandler (MouseButtonEventHandler mouseDownHandler)
                nozzleClicks |> snapshotAndTakeC nozzleLoop |> mapS (fun n -> match n with | Down -> Up | Up -> Down) |> holdS Down
            let struct (_, (sSaleComplete, delivery)) = loopC (fun nozzle1 ->
                let struct (_, (sSaleComplete, delivery)) = loopC (fun nozzle2 ->
                    let struct (_, (sSaleComplete, delivery)) = loopC (fun nozzle3 ->
                        let outputs = logic.SelectedItem |> mapC (fun (pump : IPump) ->
                            pump.Create
                                ({
                                    sNozzle1 = nozzle1 |> updatesC;
                                    sNozzle2 = nozzle2 |> updatesC;
                                    sNozzle3 = nozzle3 |> updatesC;
                                    sKeypad = sKey;
                                    sFuelPulses = sFuelPulses;
                                    calibration = calibration;
                                    price1 = price1;
                                    price2 = price2;
                                    price3 = price3;
                                    sClearSale = sClearSale
                                }))
                        let delivery = outputs |> mapC (fun o -> o.delivery) |> switchC
                        let presetLcd = outputs |> mapC (fun o -> o.presetLcd) |> switchC
                        let saleCostLcd = outputs |> mapC (fun o -> o.saleCostLcd) |> switchC
                        let saleQuantityLcd = outputs |> mapC (fun o -> o.saleQuantityLcd) |> switchC
                        let priceLcd1 = outputs |> mapC (fun o -> o.priceLcd1) |> switchC
                        let priceLcd2 = outputs |> mapC (fun o -> o.priceLcd2) |> switchC
                        let priceLcd3 = outputs |> mapC (fun o -> o.priceLcd3) |> switchC
                        let sBeep = outputs |> mapC (fun o -> o.sBeep) |> switchS
                        let sSaleComplete = outputs |> mapC (fun o -> o.sSaleComplete) |> switchS
                        
                        let startBackgroundThread (a : unit -> unit) =
                            let thread = Thread(a)
                            thread.IsBackground <- true
                            thread.Start()
                        let getResourceStream path =
                            let r = Application.GetResourceStream (Uri (path, UriKind.Relative))
                            if obj.ReferenceEquals (r, null) then invalidOp (sprintf "Could not find %s resource." path)
                            r.Stream
                        let beepPlayer = new SoundPlayer(getResourceStream @"sounds\beep.wav")
                        listeners.Add (sBeep |> listenS (fun _ -> startBackgroundThread (fun () -> beepPlayer.PlaySync ())))
                        let fastRumblePlayer = new SoundPlayer(getResourceStream @"sounds\fast.wav")
                        let mutable stopFast = fun () -> ()
                        let playFast () =
                            let mre = new ManualResetEvent false
                            startBackgroundThread (fun () ->
                                fastRumblePlayer.PlayLooping ()
                                mre.WaitOne () |> ignore
                                fastRumblePlayer.Stop ())
                            stopFast <- (fun () ->
                                mre.Set () |> ignore
                                stopFast <- fun () -> ())
                        let slowRumblePlayer = new SoundPlayer(getResourceStream @"sounds\slow.wav")
                        let mutable stopSlow = fun () -> ()
                        let playSlow () =
                            let mre = new ManualResetEvent false 
                            startBackgroundThread (fun () ->
                                slowRumblePlayer.PlayLooping ()
                                mre.WaitOne () |> ignore
                                slowRumblePlayer.Stop ())
                            stopSlow <- (fun () ->
                                mre.Set () |> ignore
                                stopSlow <- fun () -> ())
                        listeners.Add (delivery |> Cell.changes |> listenS (fun d ->
                            this.Dispatcher.InvokeAsync (fun () ->
                                match d with
                                | Fast1 | Fast2 | Fast3 ->
                                    playFast ()
                                    stopSlow ()
                                | Slow1 | Slow2 | Slow3 ->
                                    playSlow ()
                                    stopFast ()
                                | Off ->
                                    stopFast ()
                                    stopSlow ()) |> ignore))
                        let addStackPanel (container : Panel) cell isLarge =
                            let stackPanel = StackPanel()
                            stackPanel.Orientation <- Orientation.Horizontal
                            container.Children.Add stackPanel |> ignore
                            listeners.Add (cell |> listenC (fun t ->
                                this.Dispatcher.InvokeAsync (fun () -> setLcdDigits stackPanel t 5 isLarge) |> ignore))
                        addStackPanel this.PresetPlaceholder presetLcd true
                        addStackPanel this.DollarsPlaceholder saleCostLcd true
                        addStackPanel this.LitersPlaceholder saleQuantityLcd true
                        addStackPanel this.Fuel1Placeholder priceLcd1 false
                        addStackPanel this.Fuel2Placeholder priceLcd2 false
                        addStackPanel this.Fuel3Placeholder priceLcd3 false
                        struct (listenAndCreateNozzle nozzle3 this.Nozzle3Image, (sSaleComplete, delivery)))
                    struct (listenAndCreateNozzle nozzle2 this.Nozzle2Image, (sSaleComplete, delivery)))
                struct (listenAndCreateNozzle nozzle1 this.Nozzle1Image, (sSaleComplete, delivery)))
            listeners.Add (sSaleComplete |> listenS (fun (sale : Sale) ->
                this.Dispatcher.InvokeAsync
                    (fun () ->
                        let dialog =
                            SaleCompleteDialog
                                (
                                    string sale.fuel,
                                    Formatters.formatPrice sale.price None,
                                    Formatters.formatSaleCost sale.cost,
                                    Formatters.formatSaleQuantity sale.quantity
                                )
                        dialog.Owner <- this
                        csClearSale |> sendC dialog.SOkClicked
                        dialog.Show ()
                        let mutable l = None
                        l <- Some (dialog.SOkClicked |> listenS (fun _ ->
                            this.Dispatcher.InvokeAsync (fun () -> dialog.Close ()) |> ignore
                            l |> Option.iter unlistenL))
                        ()) |> ignore))
            this.listener <- Listener.fromList listeners
            
            let rec fuelPulseLoop () =
                async {
                    runT (fun () ->
                        match delivery |> sampleC with
                        | Fast1 | Fast2 | Fast3 -> sFuelPulses |> sendS 40
                        | Slow1 | Slow2 | Slow3 -> sFuelPulses |> sendS 2
                        | _ -> ())
                    do! Async.Sleep 200
                    return! fuelPulseLoop ()
                }
            fuelPulseLoop () |> Async.Start
    
    interface IDisposable with
        member this.Dispose () = this.listener |> unlistenL