using System;
using System.Windows.Controls;
using Sodium.Functional;

namespace Battle
{
    public class Classic : IParadigm
    {
        private readonly Action<string> addMessage;
        private Maybe<DragInfo> dragInfo;

        public Classic(Action<string> addMessage)
        {
            this.addMessage = addMessage;
        }

        public void HandleMouseDown(MouseEvtWithElement me)
        {
            this.addMessage("classic dragging " + me.Element.Name);
            this.dragInfo = Maybe.Some(new DragInfo(me, Canvas.GetLeft(me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Element.Polygon).ZeroIfNaN()));
        }

        public void HandleMouseMove(MouseEvt me)
        {
            this.dragInfo.MatchSome(
                d =>
                {
                    Reposition r = new Reposition(d, me);
                    Canvas.SetLeft(r.Polygon, r.Left);
                    Canvas.SetTop(r.Polygon, r.Top);
                });
        }

        public void HandleMouseUp(MouseEvt me)
        {
            this.dragInfo = Maybe.None;
        }

        public void Dispose()
        {
        }
    }
}