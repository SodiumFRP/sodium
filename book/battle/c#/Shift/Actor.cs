using System;
using System.Collections.Concurrent;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Controls;
using System.Windows.Threading;

namespace Shift
{
    public class Actor : IParadigm
    {
        private readonly Action<string> addMessage;

        private readonly CancellationTokenSource cts = new CancellationTokenSource();
        private readonly BlockingCollection<object> @in = new BlockingCollection<object>(1);

        public Actor(Action<string> addMessage, Dispatcher dispatcher)
        {
            this.addMessage = addMessage;

            BlockingCollection<Reposition> @out = new BlockingCollection<Reposition>(1);

            Task.Run(() =>
            {
                try
                {
                    while (true)
                    {
                        this.cts.Token.ThrowIfCancellationRequested();

                        bool axisLock = false;

                        DragInfo dragInfo;
                        while (true)
                        {
                            this.cts.Token.ThrowIfCancellationRequested();

                            object e = this.@in.Take(this.cts.Token);
                            EvtDown me = e as EvtDown;
                            ShiftEvt se = e as ShiftEvt;
                            if (me != null)
                            {
                                dragInfo = dispatcher.Invoke(() => new DragInfo(me.Me, Canvas.GetLeft(me.Me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Me.Element.Polygon).ZeroIfNaN()));
                                break;
                            }
                            if (se != null)
                            {
                                axisLock = se.IsDown;
                            }
                        }

                        dispatcher.Invoke(() => this.addMessage("actor dragging " + dragInfo.Me.Element.Name));
                        while (true)
                        {
                            this.cts.Token.ThrowIfCancellationRequested();

                            object me = this.@in.Take(this.cts.Token);

                            EvtUp meUp = me as EvtUp;
                            EvtMove meMove = me as EvtMove;
                            ShiftEvt se = me as ShiftEvt;
                            if (meUp != null)
                            {
                                break;
                            }
                            if (meMove != null)
                            {
                                @out.Add(new Reposition(dragInfo, meMove.Me, axisLock), this.cts.Token);
                            }
                            else if (se != null)
                            {
                                axisLock = se.IsDown;
                            }
                        }
                    }
                }
                catch (OperationCanceledException)
                {
                }
            });

            Task.Run(() =>
            {
                try
                {
                    while (true)
                    {
                        this.cts.Token.ThrowIfCancellationRequested();

                        Reposition r = @out.Take(this.cts.Token);
                        dispatcher.Invoke(() =>
                        {
                            Canvas.SetLeft(r.Polygon, r.Left);
                            Canvas.SetTop(r.Polygon, r.Top);
                        });
                    }
                }
                catch (OperationCanceledException)
                {
                }
            });
        }

        public void HandleMouseDown(MouseEvtWithElement me)
        {
            Task.Run(() => this.@in.Add(new EvtDown(me), this.cts.Token));
        }

        public void HandleMouseMove(MouseEvt me)
        {
            Task.Run(() => this.@in.Add(new EvtMove(me), this.cts.Token));
        }

        public void HandleMouseUp(MouseEvt me)
        {
            Task.Run(() => this.@in.Add(new EvtUp(me), this.cts.Token));
        }

        public void HandleShift(bool isDown)
        {
            Task.Run(() => this.@in.Add(new ShiftEvt(isDown), this.cts.Token));
        }

        public void Dispose()
        {
            this.cts.Cancel();
        }

        private class EvtDown : EvtBase
        {
            public EvtDown(MouseEvtWithElement me)
                : base(me)
            {
                this.Me = me;
            }

            public new MouseEvtWithElement Me { get; }
        }

        private class EvtMove : EvtBase
        {
            public EvtMove(MouseEvt me)
                : base(me)
            {
            }
        }

        private class EvtUp : EvtBase
        {
            public EvtUp(MouseEvt me)
                : base(me)
            {
            }
        }

        private abstract class EvtBase
        {
            protected EvtBase(MouseEvt me)
            {
                this.Me = me;
            }

            public MouseEvt Me { get; }
        }

        private class ShiftEvt
        {
            public ShiftEvt(bool isDown)
            {
                this.IsDown = isDown;
            }

            public bool IsDown { get; }
        }
    }
}