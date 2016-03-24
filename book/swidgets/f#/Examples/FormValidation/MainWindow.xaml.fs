namespace FormValidation

open System
open System.Windows.Controls
open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let emailAndValidationPlaceholders =
            [
                view.Email1Placeholder, view.Email1ValidationPlaceholder;
                view.Email2Placeholder, view.Email2ValidationPlaceholder;
                view.Email3Placeholder, view.Email3ValidationPlaceholder;
                view.Email4Placeholder, view.Email4ValidationPlaceholder;
            ]
        let maxEmails = emailAndValidationPlaceholders.Length

        let name = new STextBox("", Width = 200.0)
        view.NamePlaceholder.Children.Add(name) |> ignore
        let nameValidationError = name.Text |> Cell.map (fun t ->
            let t = t.Trim()
            if String.IsNullOrEmpty(t) then "<-- enter something"
            elif t.IndexOf(' ') < 0 then "<-- must contain space"
            else "")
        let isNameValid = nameValidationError |> Cell.map String.IsNullOrEmpty
        let validName = new SLabel(nameValidationError)
        view.NameValidationPlaceholder.Children.Add(validName) |> ignore

        let number = SSpinner.create 1
        view.NumberOfEmailAddressesPlaceholder.Children.Add(number) |> ignore
        let numberOfEmailAddressesValidationError = number.Value |> Cell.map (fun n ->
            if n < 1 || n > maxEmails then sprintf "<-- must be 1 to %i" maxEmails else "")
        let isNumberOfEmailAddressesValid = numberOfEmailAddressesValidationError |> Cell.map String.IsNullOrEmpty
        let validNumber = new SLabel(numberOfEmailAddressesValidationError)
        view.NumberOfEmailAddressesValidationPlaceholder.Children.Add(validNumber) |> ignore

        let getValidEmail i (e : Grid, v : Grid) =
            let enabled = number.Value |> Cell.map ((<) i)
            let email = new STextBox("", enabled, Width = 200.0)
            e.Children.Add(email) |> ignore
            let validateEmail (e : string) n =
                let e = e.Trim()
                if i >= n then ""
                elif String.IsNullOrEmpty(e) then "<-- enter something"
                elif e.IndexOf('@') < 0 then "<-- must contain @"
                else ""
            let validText = Cell.lift2 validateEmail email.Text number.Value
            let validEmail = new SLabel(validText)
            v.Children.Add(validEmail) |> ignore
            validText |> Cell.map String.IsNullOrEmpty
        let validEmails = emailAndValidationPlaceholders |> List.mapi getValidEmail

        let allValid = Cell.liftAll (List.forall id) (validEmails |> List.append [ isNameValid; isNumberOfEmailAddressesValid ])
        let ok = new SButton(allValid, Content = "OK", Width = 75.0)
        view.ButtonPlaceholder.Children.Add(ok) |> ignore