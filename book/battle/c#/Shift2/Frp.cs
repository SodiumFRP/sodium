using System;
using System.Windows.Controls;
using Sodium;

namespace Shift2
{
    public class Frp : IParadigm
    {
        private readonly IListener listener;

        private readonly StreamSink<MouseEvtWithElement> sMouseDown = new StreamSink<MouseEvtWithElement>();
        private readonly StreamSink<MouseEvt> sMouseMove = new StreamSink<MouseEvt>();
        private readonly StreamSink<MouseEvt> sMouseUp = new StreamSink<MouseEvt>();
        private readonly StreamSink<bool> sShift = new StreamSink<bool>();

        public Frp(Action<string> addMessage)
        {
            this.listener = Transaction.Run(() =>
            {
                Cell<IMaybe<DragInfo>> dragInfo =
                    this.sMouseDown.Map(me => Maybe.Just(new DragInfo(me, Canvas.GetLeft(me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Element.Polygon).ZeroIfNaN())))
                        .OrElse(this.sMouseUp.Map(_ => Maybe.Nothing<DragInfo>())).Hold(Maybe.Nothing<DragInfo>());
                Cell<bool> axisLock = this.sShift.Hold(false);
                Cell<IMaybe<Tuple<MouseEvt, bool>>> mouseMoveAndAxisLock = dragInfo.Map(md => md.Match(
                    d => this.sMouseMove.Hold(d.Me).Lift(axisLock, Tuple.Create).Map(Maybe.Just),
                    () => Cell.Constant(Maybe.Nothing<Tuple<MouseEvt, bool>>()))).SwitchC();
                IListener listener1 = Operational.Value(dragInfo).FilterMaybe().Listen(d => addMessage("FRP dragging " + d.Me.Element.Name));
                IListener listener2 = Operational.Value(mouseMoveAndAxisLock).FilterMaybe().Snapshot(dragInfo, (ma, md) => md.Match(
                    d => Maybe.Just(new Reposition(d, ma.Item1, ma.Item2)),
                    Maybe.Nothing<Reposition>)).FilterMaybe().Listen(p =>
                    {
                        Canvas.SetLeft(p.Polygon, p.Left);
                        Canvas.SetTop(p.Polygon, p.Top);
                    });
                return new ImmutableCompositeListener(new[] { listener1, listener2 });
            });
        }

        public void HandleMouseDown(MouseEvtWithElement me) => this.sMouseDown.Send(me);
        public void HandleMouseMove(MouseEvt me) => this.sMouseMove.Send(me);
        public void HandleMouseUp(MouseEvt me) => this.sMouseUp.Send(me);
        public void HandleShift(bool isDown) => this.sShift.Send(isDown);

        public void Dispose()
        {
            using (this.listener)
            {
            }
        }
    }
}