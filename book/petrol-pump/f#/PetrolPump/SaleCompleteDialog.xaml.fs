namespace PetrolPump

open FsXaml

type SaleCompleteDialogBase = XAML<"SaleCompleteDialog.xaml">

type SaleCompleteDialog(fuel, price, dollarsDelivered, litersDelivered) as this =
    inherit SaleCompleteDialogBase()
    
    do
        this.FuelTextBlock.Text <- fuel
        this.PriceTextBlock.Text <- price
        this.DollarsDeliveredTextBlock.Text <- dollarsDelivered
        this.LitersDeliveredTextBlock.Text <- litersDelivered
    
    member this.SOkClicked = this.OkButton.SClicked