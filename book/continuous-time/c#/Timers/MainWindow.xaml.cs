using System;
using System.Threading.Tasks;
using System.Windows.Controls;
using Sodium;
using Sodium.Time;

namespace Timers
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            this.Loaded += async (sender, args) =>
            {
                TimerSystem<DateTime> sys = new SystemClockTimerSystem(e => this.Dispatcher.Invoke(() => { throw e; }));
                Cell<DateTime> time = sys.Time;
                StreamSink<Unit> sMain = new StreamSink<Unit>();
                IListener l = Transaction.Run(() =>
                {
                    DateTime t0 = time.Sample();
                    IListener l1 = Periodic(sys, TimeSpan.FromSeconds(1)).Listen(t => this.AddMessage(t - t0 + " timer"));
                    IListener l2 = sMain.Snapshot(time).Listen(t => this.AddMessage(t - t0 + " main"));
                    return new ImmutableCompositeListener(new[] { l1, l2 });
                });
                for (int i = 0; i < 5; i++)
                {
                    sMain.Send(Unit.Value);
                    await Task.Delay(990);
                }
                l.Unlisten();
            };
        }

        private static Stream<DateTime> Periodic(TimerSystem<DateTime> sys, TimeSpan period)
        {
            Cell<DateTime> time = sys.Time;
            CellLoop<IMaybe<DateTime>> oAlarm = new CellLoop<IMaybe<DateTime>>();
            Stream<DateTime> sAlarm = sys.At(oAlarm);
            oAlarm.Loop(sAlarm.Map(t => Maybe.Just(t + period)).Hold(Maybe.Just(time.Sample() + period)));
            return sAlarm;
        }

        private void AddMessage(string message)
        {
            Action action = () =>
            {
                this.StackPanel.Children.Add(new TextBlock { Text = message });
                this.ScrollViewer.ScrollToBottom();
            };

            if (this.Dispatcher.CheckAccess())
            {
                action();
            }
            else
            {
                this.Dispatcher.Invoke(action);
            }
        }
    }
}