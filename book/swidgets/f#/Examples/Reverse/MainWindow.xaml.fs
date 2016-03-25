namespace Reverse

open System
open System.Linq
open System.Windows
open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let msg = new STextBox("Hello", Width = 150.0)
        let reverseString (s : string) = String((s :> char seq).Reverse().ToArray())
        let reversed = msg.Text |> Cell.map reverseString
        let lbl = new SLabel(reversed, Width = 150.0, Margin = Thickness(5.0, 0.0, 0.0, 0.0))

        view.Container.Children.Add(msg) |> ignore
        view.Container.Children.Add(lbl) |> ignore