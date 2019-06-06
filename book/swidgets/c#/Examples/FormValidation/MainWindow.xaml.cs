using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Controls;
using Sodium.Frp;
using SWidgets;

namespace FormValidation
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            IReadOnlyList<Tuple<Grid, Grid>> emailAndValidationPlaceholders = new[]
            {
                Tuple.Create(this.Email1Placeholder, this.Email1ValidationPlaceholder),
                Tuple.Create(this.Email2Placeholder, this.Email2ValidationPlaceholder),
                Tuple.Create(this.Email3Placeholder, this.Email3ValidationPlaceholder),
                Tuple.Create(this.Email4Placeholder, this.Email4ValidationPlaceholder)
            };
            int maxEmails = emailAndValidationPlaceholders.Count;

            STextBox name = new STextBox(string.Empty) { Width = 200 };
            this.NamePlaceholder.Children.Add(name);
            Cell<string> nameValidationError = name.Text.Map(t => string.IsNullOrEmpty(t.Trim()) ? "<-- enter something" : t.Trim().IndexOf(' ') < 0 ? "<-- must contain space" : string.Empty);
            Cell<bool> isNameValid = nameValidationError.Map(string.IsNullOrEmpty);
            SLabel validName = new SLabel(nameValidationError);
            this.NameValidationPlaceholder.Children.Add(validName);

            SSpinner number = SSpinner.Create(1);
            this.NumberOfEmailAddressesPlaceholder.Children.Add(number);
            Cell<string> numberOfEmailAddressesValidationError = number.Value.Map(n => n < 1 || n > maxEmails ? "<-- must be 1 to " + maxEmails : string.Empty);
            Cell<bool> isNumberOfEmailAddressesValid = numberOfEmailAddressesValidationError.Map(string.IsNullOrEmpty);
            SLabel validNumber = new SLabel(numberOfEmailAddressesValidationError);
            this.NumberOfEmailAddressesValidationPlaceholder.Children.Add(validNumber);

            IReadOnlyList<Cell<bool>> validEmails = emailAndValidationPlaceholders.Select((p, i) =>
            {
                Cell<bool> enabled = number.Value.Map(n => i < n);
                STextBox email = new STextBox(string.Empty, enabled) { Width = 200 };
                p.Item1.Children.Add(email);
                Cell<string> validText = email.Text.Lift(number.Value, (e, n) => i >= n ? string.Empty : string.IsNullOrEmpty(e.Trim()) ? "<-- enter something" : e.IndexOf('@') < 0 ? "<-- must contain @" : string.Empty);
                SLabel validEmail = new SLabel(validText);
                p.Item2.Children.Add(validEmail);
                return validText.Map(string.IsNullOrEmpty);
            }).ToArray();

            Cell<bool> allValid = validEmails.Concat(new[] { isNameValid, isNumberOfEmailAddressesValid }).Lift(vv => vv.All(v => v));
            SButton ok = new SButton(allValid) { Content = "OK", Width = 75 };
            this.ButtonPlaceholder.Children.Add(ok);
        }
    }
}