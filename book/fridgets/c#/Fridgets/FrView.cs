using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using Sodium;

namespace Fridgets
{
    public class FrView : Canvas, IDisposable
    {
        private readonly IListener l;
        private readonly CellSink<IMaybe<Size>> size;
        private readonly Cell<DrawableDelegate> drawable;

        public FrView(Window window, Fridget fr, IListener l)
        {
            StreamSink<MouseEvent> sMouse = new StreamSink<MouseEvent>();
            StreamSink<KeyEvent> sKey = new StreamSink<KeyEvent>();
            this.MouseDown += (sender, args) => sMouse.Send(new MouseEvent(args, () => args.GetPosition(this)));
            this.MouseUp += (sender, args) => sMouse.Send(new MouseEvent(args, () => args.GetPosition(this)));
            this.MouseMove += (sender, args) => sMouse.Send(new MouseEvent(args, () => args.GetPosition(this)));
            this.size = new CellSink<IMaybe<Size>>(Maybe.Nothing<Size>());
            this.SizeChanged += (sender, args) => this.size.Send(Maybe.Just(args.NewSize));
            window.KeyDown += (sender, args) =>
            {
                Key key = args.Key == Key.System ? args.SystemKey : args.Key;
                if (key == Key.Back)
                {
                    sKey.Send(new BackspaceKeyEvent());
                }
            };
            window.TextInput += (sender, args) => sKey.Send(new StringKeyEvent(args.Text));
            CellLoop<long> focus = new CellLoop<long>();
            Fridget.Output fo = fr.Reify(this.size, sMouse, sKey, focus, new Supply());
            focus.Loop(fo.SChangeFocus.Hold(-1));
            this.drawable = fo.Drawable;
            this.l = new ImmutableCompositeListener(new[] { l, Operational.Updates(this.drawable).Listen(d => this.InvalidateVisual()) });
        }

        protected override void OnRender(DrawingContext dc)
        {
            base.OnRender(dc);

            this.drawable.Sample()(dc);
        }

        public void Dispose()
        {
            using (this.l)
            {
            }
        }
    }
}