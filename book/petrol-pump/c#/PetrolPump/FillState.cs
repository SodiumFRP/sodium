namespace PetrolPump
{
    public class FillState
    {
        public FillState(FillStateType mode)
        {
            this.Mode = mode;
            this.Sale = null;
        }

        public FillState(FillStateType mode, Sale sale)
        {
            this.Mode = mode;
            this.Sale = sale;
        }

        public FillStateType Mode { get; }
        public Sale Sale { get; }
    }
}