namespace FormValidation

open System
open System.Windows.Controls
open FsXaml
open Sodium.Frp
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let emailAndValidationPlaceholders =
                [
                    this.Email1Placeholder, this.Email1ValidationPlaceholder;
                    this.Email2Placeholder, this.Email2ValidationPlaceholder;
                    this.Email3Placeholder, this.Email3ValidationPlaceholder;
                    this.Email4Placeholder, this.Email4ValidationPlaceholder;
                ]
            let maxEmails = emailAndValidationPlaceholders.Length
    
            let name = new STextBox("", Width = 200.0)
            this.NamePlaceholder.Children.Add(name) |> ignore
            let nameValidationError = name.Text |> mapC (fun t ->
                let t = t.Trim()
                if String.IsNullOrEmpty(t) then "<-- enter something"
                elif t.IndexOf(' ') < 0 then "<-- must contain space"
                else "")
            let isNameValid = nameValidationError |> mapC String.IsNullOrEmpty
            let validName = new SLabel(nameValidationError)
            this.NameValidationPlaceholder.Children.Add(validName) |> ignore
    
            let number = SSpinner.create 1
            this.NumberOfEmailAddressesPlaceholder.Children.Add(number) |> ignore
            let numberOfEmailAddressesValidationError = number.Value |> mapC (fun n ->
                if n < 1 || n > maxEmails then sprintf "<-- must be 1 to %i" maxEmails else "")
            let isNumberOfEmailAddressesValid = numberOfEmailAddressesValidationError |> mapC String.IsNullOrEmpty
            let validNumber = new SLabel(numberOfEmailAddressesValidationError)
            this.NumberOfEmailAddressesValidationPlaceholder.Children.Add(validNumber) |> ignore
    
            let getValidEmail i (e : Grid, v : Grid) =
                let enabled = number.Value |> mapC ((<) i)
                let email = new STextBox("", enabled, Width = 200.0)
                e.Children.Add(email) |> ignore
                let validateEmail (e : string) n =
                    let e = e.Trim()
                    if i >= n then ""
                    elif String.IsNullOrEmpty(e) then "<-- enter something"
                    elif e.IndexOf('@') < 0 then "<-- must contain @"
                    else ""
                let validText = (email.Text, number.Value) |> lift2C validateEmail
                let validEmail = new SLabel(validText)
                v.Children.Add(validEmail) |> ignore
                validText |> mapC String.IsNullOrEmpty
            let validEmails = emailAndValidationPlaceholders |> List.mapi getValidEmail
    
            let allValid = validEmails |> List.append [ isNameValid; isNumberOfEmailAddressesValid ] |> liftAllC (Seq.forall id)
            let ok = new SButton(allValid, Content = "OK", Width = 75.0)
            this.ButtonPlaceholder.Children.Add(ok) |> ignore