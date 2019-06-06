namespace Reverse

open System
open System.Linq
open System.Windows
open FsXaml
open Sodium.Frp
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let msg = new STextBox("Hello", Width = 150.0)
            let reverseString (s : string) = String((s :> char seq).Reverse().ToArray())
            let reversed = msg.Text |> Cell.map reverseString
            let lbl = new SLabel(reversed, Width = 150.0, Margin = Thickness(5.0, 0.0, 0.0, 0.0))
    
            this.Container.Children.Add(msg) |> ignore
            this.Container.Children.Add(lbl) |> ignore