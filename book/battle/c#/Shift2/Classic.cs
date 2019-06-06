using System;
using System.Windows.Controls;
using Sodium.Functional;

namespace Shift2
{
    public class Classic : IParadigm
    {
        private readonly Action<string> addMessage;
        private Maybe<DragInfo> dragInfo;

        private Maybe<MouseEvt> lastMe;
        private bool axisLock;

        public Classic(Action<string> addMessage)
        {
            this.addMessage = addMessage;
        }

        public void HandleMouseDown(MouseEvtWithElement me)
        {
            this.lastMe = Maybe.Some(me.Upcast<MouseEvt>());

            this.addMessage("classic dragging " + me.Element.Name);
            this.dragInfo = Maybe.Some(new DragInfo(me, Canvas.GetLeft(me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Element.Polygon).ZeroIfNaN()));
        }

        public void HandleMouseMove(MouseEvt me)
        {
            this.lastMe = Maybe.Some(me);
            this.Reposition();
        }

        public void HandleMouseUp(MouseEvt me)
        {
            this.dragInfo = Maybe.None;
        }

        public void HandleShift(bool isDown)
        {
            this.axisLock = isDown;
            this.Reposition();
        }

        private void Reposition()
        {
            this.dragInfo.MatchSome(
                d =>
                {
                    this.lastMe.MatchSome(
                        me =>
                        {
                            Reposition r = new Reposition(d, me, this.axisLock);
                            Canvas.SetLeft(r.Polygon, r.Left);
                            Canvas.SetTop(r.Polygon, r.Top);
                        });
                });
        }

        public void Dispose()
        {
        }
    }
}