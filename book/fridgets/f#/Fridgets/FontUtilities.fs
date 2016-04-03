module Fridgets.FontUtilities

open System.Globalization
open System.Windows
open System.Windows.Media

let getStandardFormattedText s typeface fontSize brush =
    FormattedText(s, CultureInfo.CurrentUICulture, FlowDirection.LeftToRight, typeface, fontSize, brush)

let measureString s typeface fontSize =
    let formattedText = getStandardFormattedText s typeface fontSize Brushes.Black
    Size(formattedText.Width, formattedText.Height)