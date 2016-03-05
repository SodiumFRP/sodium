namespace Fridgets
{
    public class StringKeyEvent : KeyEvent
    {
        public StringKeyEvent(string s)
        {
            this.String = s;
        }

        public string String { get; }
    }
}