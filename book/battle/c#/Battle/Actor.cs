using System;
using System.Collections.Concurrent;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Controls;
using System.Windows.Threading;

namespace Battle
{
    public class Actor : IParadigm
    {
        private readonly Action<string> addMessage;

        private readonly CancellationTokenSource cts = new CancellationTokenSource();
        private readonly BlockingCollection<EvtBase> @in = new BlockingCollection<EvtBase>(1);

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

                        DragInfo dragInfo;
                        while (true)
                        {
                            this.cts.Token.ThrowIfCancellationRequested();

                            EvtDown me = this.@in.Take(this.cts.Token) as EvtDown;
                            if (me != null)
                            {
                                dragInfo = dispatcher.Invoke(() => new DragInfo(me.Me, Canvas.GetLeft(me.Me.Element.Polygon).ZeroIfNaN(), Canvas.GetTop(me.Me.Element.Polygon).ZeroIfNaN()));
                                break;
                            }
                        }

                        dispatcher.Invoke(() => this.addMessage("actor dragging " + dragInfo.Me.Element.Name));
                        while (true)
                        {
                            this.cts.Token.ThrowIfCancellationRequested();

                            EvtBase me = this.@in.Take(this.cts.Token);

                            EvtUp meUp = me as EvtUp;
                            EvtMove meMove = me as EvtMove;
                            if (meUp != null)
                            {
                                break;
                            }
                            if (meMove != null)
                            {
                                @out.Add(new Reposition(dragInfo, meMove.Me), this.cts.Token);
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
    }
}