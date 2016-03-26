namespace GameChat

open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let sOnegai = view.OnegaiButton.SClicked |> Stream.mapTo "Onegai shimasu"
        let sThanks = view.ThanksButton.SClicked |> Stream.mapTo "Thank you"
        let sCanned = sOnegai |> Stream.orElse sThanks

        let text = new STextBox(sCanned, "")
        view.TextPlaceholder.Children.Add(text) |> ignore