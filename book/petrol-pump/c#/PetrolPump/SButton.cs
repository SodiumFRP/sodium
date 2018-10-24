using System.Threading.Tasks;
using System.Windows.Controls;
using SodiumFRP;

namespace PetrolPump
{
    public class SButton : Button
    {
        public SButton()
        {
            StreamSink<Unit> sClickedSink = Stream.CreateSink<Unit>();
            this.SClicked = sClickedSink;
            this.Click += async (sender, args) => await Task.Run(() => sClickedSink.Send(Unit.Value));
        }

        public Stream<Unit> SClicked { get; }
    }
}