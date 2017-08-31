using System;
using System.Windows.Controls;
using Sodium;

namespace Shift2
{
    public class Classic : IParadigm
    {
        private readonly Action<string> addMessage;
        private readonly MutableMaybeValue<DragInfo> dragInfo = new MutableMaybeValue<DragInfo>();

        private IMaybe<MouseEvt> lastMe = Maybe.Nothing<MouseEvt>();
        private bool axisLock;

        public Classic(Action<string> addMessage)
        {
            this.addMessage = addMessage;
        }

        public void HandleMouseDown(MouseEvtWithElement me)
        {
            this.lastMe = Maybe.Just(me);

            this.addMessage("classic dragging " + me.Element.Name);
            this.dragInfo.Set(new DragInfo(me, Canvas.GetLeft(me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Element.Polygon).ZeroIfNaN()));
        }

        public void HandleMouseMove(MouseEvt me)
        {
            this.lastMe = Maybe.Just(me);
            this.Reposition();
        }

        public void HandleMouseUp(MouseEvt me)
        {
            this.dragInfo.Reset();
        }

        public void HandleShift(bool isDown)
        {
            this.axisLock = isDown;
            this.Reposition();
        }

        private void Reposition()
        {
            this.dragInfo.Match(
                d =>
                {
                    this.lastMe.Match(
                        me =>
                        {
                            Reposition r = new Reposition(d, me, this.axisLock);
                            Canvas.SetLeft(r.Polygon, r.Left);
                            Canvas.SetTop(r.Polygon, r.Top);
                        },
                        () => { });
                },
                () => { });
        }

        public void Dispose()
        {
        }
    }
}