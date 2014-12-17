namespace PetrolPump.Pump
{
  public class FillState 
  {
    public enum Type 
    {
        IDLE,
        FILLING,
        SALE_COMPLETE
    }

    public FillState(Type mode) 
    {
        this.Mode = mode;
        this.Sale = null;
    }

    public FillState(Type mode, Sale sale) 
    {
        this.Mode = mode;
        this.Sale = sale;
    }

    public readonly Type Mode;
    public readonly Sale Sale;
}

}
