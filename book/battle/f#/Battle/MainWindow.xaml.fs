namespace Battle

open System
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open FSharpx.Functional.Prelude
open FsXaml
open Sodium

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
        ()