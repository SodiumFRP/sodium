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
        private readonly DiscreteCell<DrawableDelegate> drawable;

        public FrView(Window window, Fridget fr, IListener l)
        {
            StreamSink<MouseEvent> sMouse = new StreamSink<MouseEvent>();
            StreamSink<KeyEvent> sKey = new StreamSink<KeyEvent>();
            this.MouseDown += (sender, args) => sMouse.Send(new MouseEvent(args, () => args.GetPosition(this)));
            this.MouseUp += (sender, args) => sMouse.Send(new MouseEvent(args, () => args.GetPosition(this)));
            this.MouseMove += (sender, args) => sMouse.Send(new MouseEvent(args, () => args.GetPosition(this)));
            DiscreteCellSink<Maybe<Size>> size = new DiscreteCellSink<Maybe<Size>>(Maybe.None);
            this.SizeChanged += (sender, args) => size.Send(Maybe.Some(args.NewSize));
            window.KeyDown += (sender, args) =>
            {
                Key key = args.Key == Key.System ? args.SystemKey : args.Key;
                if (key == Key.Back)
                {
                    sKey.Send(new BackspaceKeyEvent());
                }
            };
            window.TextInput += (sender, args) => sKey.Send(new StringKeyEvent(args.Text));
            DiscreteCellLoop<long> focus = new DiscreteCellLoop<long>();
            Fridget.Output fo = fr.Reify(size, sMouse, sKey, focus, new Supply());
            focus.Loop(fo.SChangeFocus.Hold(-1));
            this.drawable = fo.Drawable;
            this.l = new CompositeListener(new[] { l, this.drawable.Updates.Listen(d => this.InvalidateVisual()) });
        }

        protected override void OnRender(DrawingContext dc)
        {
            base.OnRender(dc);

            this.drawable.Cell.Sample()(dc);
        }

        public void Dispose()
        {
            this.l.Unlisten();
        }
    }
}