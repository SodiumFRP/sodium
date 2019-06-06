using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump
{
    public class Outputs
    {
        private Outputs(
            Cell<Delivery> delivery,
            Cell<string> presetLcd,
            Cell<string> saleCostLcd,
            Cell<string> saleQuantityLcd,
            Cell<string> priceLcd1,
            Cell<string> priceLcd2,
            Cell<string> priceLcd3,
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
            this.Delivery = Cell.Constant(PetrolPump.Delivery.Off);
            this.PresetLcd = Cell.Constant(string.Empty);
            this.SaleCostLcd = Cell.Constant(string.Empty);
            this.SaleQuantityLcd = Cell.Constant(string.Empty);
            this.PriceLcd1 = Cell.Constant(string.Empty);
            this.PriceLcd2 = Cell.Constant(string.Empty);
            this.PriceLcd3 = Cell.Constant(string.Empty);
            this.SBeep = Stream.Never<Unit>();
            this.SSaleComplete = Stream.Never<Sale>();
        }

        public Cell<Delivery> Delivery { get; }
        public Cell<string> PresetLcd { get; }
        public Cell<string> SaleCostLcd { get; }
        public Cell<string> SaleQuantityLcd { get; }
        public Cell<string> PriceLcd1 { get; }
        public Cell<string> PriceLcd2 { get; }
        public Cell<string> PriceLcd3 { get; }
        public Stream<Unit> SBeep { get; }
        public Stream<Sale> SSaleComplete { get; }

        public Outputs SetDelivery(Cell<Delivery> delivery)
        {
            return new Outputs(delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetPresetLcd(Cell<string> presetLcd)
        {
            return new Outputs(this.Delivery, presetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetSaleCostLcd(Cell<string> saleCostLcd)
        {
            return new Outputs(this.Delivery, this.PresetLcd, saleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetSaleQuantityLcd(Cell<string> saleQuantityLcd)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                saleQuantityLcd, this.PriceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetPriceLcd1(Cell<string> priceLcd1)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, priceLcd1, this.PriceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetPriceLcd2(Cell<string> priceLcd2)
        {
            return new Outputs(this.Delivery, this.PresetLcd, this.SaleCostLcd,
                this.SaleQuantityLcd, this.PriceLcd1, priceLcd2, this.PriceLcd3, this.SBeep,
                this.SSaleComplete);
        }

        public Outputs SetPriceLcd3(Cell<string> priceLcd3)
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