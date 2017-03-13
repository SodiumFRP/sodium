using Sodium;

namespace PetrolPump
{
    public class Outputs
    {
        private Outputs(
            DiscreteCell<Delivery> delivery,
            DiscreteCell<string> presetLcd,
            DiscreteCell<string> saleCostLcd,
            DiscreteCell<string> saleQuantityLcd,
            DiscreteCell<string> priceLcd1,
            DiscreteCell<string> priceLcd2,
            DiscreteCell<string> priceLcd3,
            Stream<Unit> sBeep,
            Stream<Sale> sSaleComplete)
        {
            this.Delivery = delivery;
            this.PresetLcd = presetLcd;
            this.SaleCostLcd = saleCostLcd;
            this.SaleQuantityLcd = saleQuantityLcd;
            this.PriceLcd1 = priceLcd1;
            this.PriceLcd2 = priceLcd2;
            this.PriceLcd3 = priceLcd3;
            this.SBeep = sBeep;
            this.SSaleComplete = sSaleComplete;
        }

        public Outputs()
        {
            this.Delivery = DiscreteCell.Constant(PetrolPump.Delivery.Off);
            this.PresetLcd = DiscreteCell.Constant(string.Empty);
            this.SaleCostLcd = DiscreteCell.Constant(string.Empty);
            this.SaleQuantityLcd = DiscreteCell.Constant(string.Empty);
            this.PriceLcd1 = DiscreteCell.Constant(string.Empty);
            this.PriceLcd2 = DiscreteCell.Constant(string.Empty);
            this.PriceLcd3 = DiscreteCell.Constant(string.Empty);
            this.SBeep = Stream.Never<Unit>();
            this.SSaleComplete = Stream.Never<Sale>();
        }

        public DiscreteCell<Delivery> Delivery { get; }
        public DiscreteCell<string> PresetLcd { get; }
        public DiscreteCell<string> SaleCostLcd { get; }
        public DiscreteCell<string> SaleQuantityLcd { get; }
        public DiscreteCell<string> PriceLcd1 { get; }
        public DiscreteCell<string> PriceLcd2 { get; }
        public DiscreteCell<string> PriceLcd3 { get; }
        public Stream<Unit> SBeep { get; }
        public Stream<Sale> SSaleComplete { get; }

        public Outputs SetDelivery(DiscreteCell<Delivery> delivery)
        {
            return new Outputs(delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetPresetLcd(DiscreteCell<string> presetLcd)
        {
            return new Outputs(this.Delivery, presetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetSaleCostLcd(DiscreteCell<string> saleCostLcd)
        {
            return new Outputs(this.Delivery, this.PresetLcd, saleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetSaleQuantityLcd(DiscreteCell<string> saleQuantityLcd)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                saleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetPriceLcd1(DiscreteCell<string> priceLcd1)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, priceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetPriceLcd2(DiscreteCell<string> priceLcd2)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, priceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetPriceLcd3(DiscreteCell<string> priceLcd3)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, priceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetBeep(Stream<Unit> sBeep)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, sBeep,
                this.SSaleComplete);
        }

        public Outputs SetSaleComplete(Stream<Sale> sSaleComplete)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                sSaleComplete);
        }
    }
}