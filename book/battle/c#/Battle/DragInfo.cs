namespace Battle
{
    public class DragInfo
    {
        public DragInfo(MouseEvtWithElement me, double originalLeft, double originalTop)
        {
            this.Me = me;
            this.OriginalLeft = originalLeft;
            this.OriginalTop = originalTop;
        }

        public MouseEvtWithElement Me { get; }
        public double OriginalLeft { get; }
        public double OriginalTop { get; }
    }
}