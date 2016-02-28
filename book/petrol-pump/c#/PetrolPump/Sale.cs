namespace PetrolPump
{
    public class Sale
    {
        public Sale(Fuel fuel, double price, double cost, double quantity)
        {
            this.Fuel = fuel;
            this.Price = price;
            this.Cost = cost;
            this.Quantity = quantity;
        }

        public Fuel Fuel { get; }
        public double Price { get; }
        public double Cost { get; }
        public double Quantity { get; }
    }
}