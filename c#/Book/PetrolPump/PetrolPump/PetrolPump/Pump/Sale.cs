namespace PetrolPump.Pump
{
  public class Sale
  {
    public Sale(Fuel fuel, double price, double cost, double quantity) {
        Fuel = fuel;
        Price = price;
        Cost = cost;
        Quantity = quantity;
    }

    public readonly Fuel Fuel;
    public readonly double Price;
    public readonly double Cost;
    public readonly double Quantity;

  }
}
