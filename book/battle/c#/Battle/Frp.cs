using System;
using System.Windows.Controls;
using Sodium;

namespace Battle
{
    public class Frp : IParadigm
    {
        private readonly IListener listener;

        private readonly StreamSink<MouseEvtWithElement> sMouseDown = new StreamSink<MouseEvtWithElement>();
        private readonly StreamSink<MouseEvt> sMouseMove = new StreamSink<MouseEvt>();
        private readonly StreamSink<MouseEvt> sMouseUp = new StreamSink<MouseEvt>();

        public Frp(Action<string> addMessage)
        {
            this.listener = Transaction.Run(() =>
            {
                DiscreteCell<Maybe<DragInfo>> dragInfo =
                    this.sMouseDown.Map(me => Maybe.Some(new DragInfo(me, Canvas.GetLeft(me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Element.Polygon).ZeroIfNaN())))
                        .OrElse(this.sMouseUp.Map(_ => Maybe<DragInfo>.None)).Hold(Maybe.None);
                Stream<MouseEvt> mouseMoveWhileDragging = dragInfo.Map(md => md.Match(d => this.sMouseMove, Stream.Never<MouseEvt>)).SwitchS();
                IListener listener1 = dragInfo.Values.FilterMaybe().Listen(d => addMessage("FRP dragging " + d.Me.Element.Name));
                IListener listener2 = mouseMoveWhileDragging.Snapshot(dragInfo, (me, md) => md.Match(d => Maybe.Some(new Reposition(d, me)), () => Maybe.None)).FilterMaybe().Listen(p =>
                {
                    Canvas.SetLeft(p.Polygon, p.Left);
                    Canvas.SetTop(p.Polygon, p.Top);
                });
                return new CompositeListener(new[] { listener1, listener2 });
            });
        }

        public void HandleMouseDown(MouseEvtWithElement me) => this.sMouseDown.Send(me);
        public void HandleMouseMove(MouseEvt me) => this.sMouseMove.Send(me);
        public void HandleMouseUp(MouseEvt me) => this.sMouseUp.Send(me);

        public void Dispose()
        {
            this.listener.Unlisten();
        }
    }
}