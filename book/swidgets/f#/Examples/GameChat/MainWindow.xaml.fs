namespace GameChat

open FsXaml
open Sodium.Frp
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let sOnegai = this.OnegaiButton.SClicked |> mapToS "Onegai shimasu"
            let sThanks = this.ThanksButton.SClicked |> mapToS "Thank you"
            let sCanned = (sOnegai, sThanks) |> orElseS
    
            let text = new STextBox(sCanned, "")
            this.TextPlaceholder.Children.Add(text) |> ignore