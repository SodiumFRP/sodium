namespace Translate

open FsXaml
open Sodium.Frp
open SWidgets
open System.Text.RegularExpressions

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let english = new STextBox("I like FRP", Width = 150.0)
            this.TextBoxPlaceholder.Children.Add(english) |> ignore
    
            let makeLatin (s : string) = Regex.Replace(s.Trim(), " |$", "us ")
            let sLatin = this.TranslateButton.SClicked |> snapshotAndTakeC english.Text |> mapS makeLatin
            let latin = sLatin |> holdS ""
            let lblLatin = new SLabel(latin)
            this.TextPlaceholder.Children.Add(lblLatin) |> ignore