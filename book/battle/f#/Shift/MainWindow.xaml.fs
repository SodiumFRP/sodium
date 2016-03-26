namespace Shift

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes
open FsXaml

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let shape ox oy sides angle =
            let polygon = Polygon(
                            Fill = SolidColorBrush(Color(R = 64uy, G = 128uy, B = 0uy, A = 255uy)),
                            Stroke = SolidColorBrush(Colors.Black),
                            StrokeThickness = 1.0)
            let angle = angle * Math.PI / 180.0
            for i = 0 to sides - 1 do
                let theta = angle + (Math.PI * 2.0 * (float i) / (float sides))
                polygon.Points.Add(Point((float ox) + Math.Sin(theta) * 25.0, (float oy) + Math.Cos(theta) * 25.0))
            polygon

        let createElements () =
            [
                { name = "triangle"; polygon = shape 50 50 3 180.0 }
                { name = "square"; polygon = shape 125 50 4 225.0 }
                { name = "pentagon"; polygon = shape 200 50 5 180.0 }
                { name = "hexagon"; polygon = shape 50 125 6 210.0 }
                { name = "heptagon"; polygon = shape 125 125 7 180.0 }
                { name = "octagon"; polygon = shape 200 125 8 202.5 }
            ]

        let addMessage message =
            view.StackPanel.Children.Add(TextBlock(Text = message)) |> ignore
            view.ScrollViewer.ScrollToBottom()
        
        let createDocumentView w c e p =
            let d = DocumentView()
            DocumentView.init d w c e p
            d

        view.ClassicPlaceholder.Child <- createDocumentView view.Root view.ClassicPlaceholder (createElements ()) (new Classic(addMessage))
        view.FrpPlaceholder.Child <- createDocumentView view.Root view.FrpPlaceholder (createElements ()) (new Frp(addMessage))
        view.ActorPlaceholder.Child <- createDocumentView view.Root view.ActorPlaceholder (createElements ()) (new Actor(addMessage, view.Root.Dispatcher))