using System;
using System.Windows.Controls;
using Sodium;

namespace Battle
{
    public class Classic : IParadigm
    {
        private readonly Action<string> addMessage;
        private readonly MutableMaybeValue<DragInfo> dragInfo = new MutableMaybeValue<DragInfo>();

        public Classic(Action<string> addMessage)
        {
            this.addMessage = addMessage;
        }

        public void HandleMouseDown(MouseEvtWithElement me)
        {
            this.addMessage("classic dragging " + me.Element.Name);
            this.dragInfo.Set(new DragInfo(me, Canvas.GetLeft(me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Element.Polygon).ZeroIfNaN()));
        }

        public void HandleMouseMove(MouseEvt me)
        {
            this.dragInfo.Match(
                d =>
                {
                    Reposition r = new Reposition(d, me);
                    Canvas.SetLeft(r.Polygon, r.Left);
                    Canvas.SetTop(r.Polygon, r.Top);
                },
                () => { });
        }

        public void HandleMouseUp(MouseEvt me)
        {
            this.dragInfo.Reset();
        }

        public void Dispose()
        {
        }
    }
}