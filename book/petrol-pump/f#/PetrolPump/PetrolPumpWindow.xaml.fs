namespace PetrolPump

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open FsXaml
open Sodium
open Sodium.Time

type PetrolPumpView = XAML<"PetrolPumpWindow.xaml", true>

type private SetLcdDigitsState =
    {
        append : Grid seq -> Grid -> Grid seq
        grids : Grid seq
        complete : Grid seq -> Grid seq
    }

type PetrolPumpViewController() =
    inherit WindowViewController<PetrolPumpView>()

    override __.OnLoaded view =
        let assemblyName = typeof<PetrolPumpViewController>.Assembly.FullName

        let (createLargeSegments, createSmallSegments) = List.init 8 (fun i ->
            let createSegment size () =
                let image = Image()
                let source = BitmapImage()
                source.BeginInit()
                source.UriSource <- Uri(sprintf "pack://application:,,,/%s;component/images/%s%i.png" assemblyName size i)
                source.EndInit()
                image.Source <- source
                image.Width <- float source.PixelWidth
                image.Height <- float source.PixelHeight
                image
            (createSegment "large", createSegment "small")) |> List.unzip

        let layouts =
            [
                [0;1;2;4;5;6];
                [2;5];
                [0;1;3;5;6];
                [0;2;3;5;6];
                [2;3;4;5];
                [0;2;3;4;6];
                [0;1;2;3;4;6];
                [2;5;6];
                [0;1;2;3;4;5;6];
                [2;3;4;5;6]
            ]

        let getCreateNumberImages layout =
            let createNumberImage createSegments () =
                let grid = Grid()
                let addSegment n = grid.Children.Add((List.item n createSegments) ()) |> ignore
                List.iter addSegment layout
                grid
            (createNumberImage createLargeSegments, createNumberImage createSmallSegments)
        let (createLargeNumberImages, createSmallNumberImages) = List.map getCreateNumberImages layouts |> List.unzip

        let createLargeDotImage () =
            let grid = Grid()
            grid.Children.Add((List.item 7 createLargeSegments) ())
            grid
        
        let createSmallDotImage () =
            let grid = Grid()
            grid.Children.Add((List.item 7 createSmallSegments) ())
            grid

        let createLargeDashImage () =
            let grid = Grid()
            grid.Children.Add((List.item 3 createLargeSegments) ())
            grid
        
        let createSmallDashImage () =
            let grid = Grid()
            grid.Children.Add((List.item 3 createSmallSegments) ())
            grid

        let getImage c isLarge =
            match c with
                | '-' -> if isLarge then createLargeDashImage () else createSmallDashImage ()
                | '0' -> List.item 0 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '1' -> List.item 1 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '2' -> List.item 2 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '3' -> List.item 3 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '4' -> List.item 4 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '5' -> List.item 5 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '6' -> List.item 6 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '7' -> List.item 7 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '8' -> List.item 8 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '9' -> List.item 9 (if isLarge then createLargeNumberImages else createSmallNumberImages) ()
                | '.' -> if isLarge then createLargeDotImage () else createSmallDotImage ()
                | _ -> invalidArg "c" "Character must be a dot, dash, or number between 0 and 9."

        let setLcdDigits (placeholder : StackPanel) text maxDigits isLarge =
            placeholder.Children.Clear()
            let defaultAppend gg g = Seq.append gg [g]
            let defaultComplete = id
            let initialState = { append = defaultAppend; grids = Seq.empty; complete = defaultComplete }
            ()
        ()