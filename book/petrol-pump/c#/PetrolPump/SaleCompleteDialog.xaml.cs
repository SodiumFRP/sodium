using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump
{
    public partial class SaleCompleteDialog
    {
        public SaleCompleteDialog(string fuel, string price, string dollarsDelivered, string litersDelivered)
        {
            this.InitializeComponent();

            this.FuelTextBlock.Text = fuel;
            this.PriceTextBlock.Text = price;
            this.DollarsDeliveredTextBlock.Text = dollarsDelivered;
            this.LitersDeliveredTextBlock.Text = litersDelivered;
        }

        public Stream<Unit> SOkClicked => this.OkButton.SClicked;
    }
}