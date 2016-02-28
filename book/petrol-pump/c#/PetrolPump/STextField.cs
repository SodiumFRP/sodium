using System.Threading.Tasks;
using System.Windows.Controls;
using Sodium;

namespace PetrolPump
{
    public class STextField : TextBox
    {
        public STextField(string initialText)
        {
            base.Text = initialText;
            CellSink<string> text = new CellSink<string>(initialText);
            this.TextChanged += async (sender, args) =>
            {
                string t = base.Text;
                await Task.Run(() => text.Send(t));
            };
            this.Text = text;
        }

        public new Cell<string> Text { get; }
    }
}