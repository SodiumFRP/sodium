using System;
using System.Windows.Controls;
using Sodium.Functional;
using Sodium.Frp;

namespace Shift2
{
    public class Frp : IParadigm
    {
        private readonly IListener listener;

        private readonly StreamSink<MouseEvtWithElement> sMouseDown = Stream.CreateSink<MouseEvtWithElement>();
        private readonly StreamSink<MouseEvt> sMouseMove = Stream.CreateSink<MouseEvt>();
        private readonly StreamSink<MouseEvt> sMouseUp = Stream.CreateSink<MouseEvt>();
        private readonly StreamSink<bool> sShift = Stream.CreateSink<bool>();

        public Frp(Action<string> addMessage)
        {
            this.listener = Transaction.Run(() =>
            {
                Cell<Maybe<DragInfo>> dragInfo =
                    this.sMouseDown.Map(me => Maybe.Some(new DragInfo(me, Canvas.GetLeft(me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Element.Polygon).ZeroIfNaN())))
                        .OrElse(this.sMouseUp.Map(_ => Maybe<DragInfo>.None)).Hold(Maybe.None);
                Cell<bool> axisLock = this.sShift.Hold(false);
                Cell<Maybe<Tuple<MouseEvt, bool>>> mouseMoveAndAxisLock = dragInfo.Map(md => md.Match(
                    d => this.sMouseMove.Hold(d.Me).Lift(axisLock, Tuple.Create).Map(Maybe.Some),
                    () => Cell.Constant(Maybe<Tuple<MouseEvt, bool>>.None))).SwitchC();
                IListener listener1 = dragInfo.Values().FilterMaybe().Listen(d => addMessage("FRP dragging " + d.Me.Element.Name));
                IListener listener2 = mouseMoveAndAxisLock
                    .Values()
                    .FilterMaybe()
                    .Snapshot(dragInfo, (ma, md) => md.Match(d => Maybe.Some(new Reposition(d, ma.Item1, ma.Item2)), () => Maybe.None))
                    .FilterMaybe()
                    .Listen(p =>
                    {
                        Canvas.SetLeft(p.Polygon, p.Left);
                        Canvas.SetTop(p.Polygon, p.Top);
                    });
                return Listener.CreateComposite(new[] { listener1, listener2 });
            });
        }

        public void HandleMouseDown(MouseEvtWithElement me) => this.sMouseDown.Send(me);
        public void HandleMouseMove(MouseEvt me) => this.sMouseMove.Send(me);
        public void HandleMouseUp(MouseEvt me) => this.sMouseUp.Send(me);
        public void HandleShift(bool isDown) => this.sShift.Send(isDown);

        public void Dispose()
        {
            this.listener.Unlisten();
        }
    }
}