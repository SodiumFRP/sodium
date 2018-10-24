namespace PetrolPump

open System
open System.Collections.Generic
open System.Media
open System.Threading
open System.Threading.Tasks
open System.Windows
open System.Windows.Controls
open System.Windows.Controls
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media
open System.Windows.Media.Imaging
open FsXaml
open PetrolPump.Controls
open SodiumFRP
open SodiumFRP.Time

type SaleCompleteDialogBase = XAML<"SaleCompleteDialog.xaml">

type SaleCompleteDialog(fuel, price, dollarsDelivered, litersDelivered) as this =
    inherit SaleCompleteDialogBase()
    
    do
        this.FuelTextBlock.Text <- fuel
        this.PriceTextBlock.Text <- price
        this.DollarsDeliveredTextBlock.Text <- dollarsDelivered
        this.LitersDeliveredTextBlock.Text <- litersDelivered
    
    member this.SOkClicked = this.OkButton.SClicked