package pump;

public class Sale
{
    public Sale(Fuel fuel, double price, double cost, double quantity) {
        this.fuel = fuel;
        this.price = price;
        this.cost = cost;
        this.quantity = quantity;
    }

    public final Fuel fuel;
    public final double price;
    public final double cost;
    public final double quantity;
}

