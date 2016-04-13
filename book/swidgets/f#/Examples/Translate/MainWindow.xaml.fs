namespace Translate

open FsXaml
open Sodium
open SWidgets
open System.Text.RegularExpressions

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let english = new STextBox("I like FRP", Width = 150.0)
        view.TextBoxPlaceholder.Children.Add(english) |> ignore

        let makeLatin (s : string) = Regex.Replace(s.Trim(), " |$", "us ")
        let sLatin = view.TranslateButton.SClicked |> Stream.snapshot (fun _ t -> makeLatin t) english.Text
        let latin = sLatin |> Stream.hold ""
        let lblLatin = new SLabel(latin)
        view.TextPlaceholder.Children.Add(lblLatin) |> ignore