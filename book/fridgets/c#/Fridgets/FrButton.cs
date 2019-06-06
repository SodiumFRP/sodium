using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using Sodium.Frp;
using Sodium.Functional;

namespace Fridgets
{
    public class FrButton : Fridget
    {
        public FrButton(Cell<string> label)
            : this(label, new StreamLoop<Unit>())
        {
        }

        private FrButton(Cell<string> label, StreamLoop<Unit> sClicked)
            : base((size, sMouse, sKey, focus, idSupply) =>
            {
                Stream<Unit> sPressed = sMouse.Snapshot(size,
                    (e, mSize) => mSize.Bind(
                        s =>
                        {
                            MouseButtonEventArgs b = e.Args as MouseButtonEventArgs;
                            Point p = e.GetPosition();
                            return b != null && b.ChangedButton == MouseButton.Left && b.ButtonState == MouseButtonState.Pressed
                                   && p.X >= 2 && p.X < s.Width - 2 && p.Y >= 2 && p.Y < s.Height - 2
                                ? Maybe.Some(Unit.Value)
                                : Maybe.None;
                        })).FilterMaybe();
                Stream<Unit> sReleased = sMouse.Snapshot(size,
                    (e, mSize) => mSize.Bind(
                        s => e.Args is MouseButtonEventArgs b && b.ChangedButton == MouseButton.Left && b.ButtonState == MouseButtonState.Released
                            ? Maybe.Some(Unit.Value)
                            : Maybe.None)).FilterMaybe();
                Cell<bool> pressed = sPressed.MapTo(true).OrElse(sReleased.MapTo(false)).Hold(false);
                sClicked.Loop(sReleased.Gate(pressed));
                Typeface typeface = new Typeface(new FontFamily("Helvetica"), FontStyles.Normal, FontWeights.Normal, FontStretches.Normal);
                Cell<Size> desiredSize = label.Map(l =>
                {
                    Size labelSize = FontUtilities.MeasureString(l, typeface, 13);
                    return new Size(labelSize.Width + 14, labelSize.Height + 10);
                });
                return new Output(
                    label.Lift(
                        size, pressed,
                        (l, mSize, p) => new DrawableDelegate(d =>
                        {
                            mSize.MatchSome(sz =>
                            {
                                d.DrawRectangle(p ? Brushes.DarkGray : Brushes.LightGray, new Pen(Brushes.Black, 1), new Rect(new Point(2, 2), new Size(sz.Width - 5, sz.Height - 5)));
                                FormattedText t = FontUtilities.GetStandardFormattedText(l, typeface, 13, Brushes.Black);
                                d.DrawText(t, new Point((sz.Width - t.Width) / 2, (sz.Height - t.Height) / 2));
                            });
                        })),
                    desiredSize,
                    Stream.Never<long>());
            })
        {
            this.SClicked = sClicked;
        }

        public Stream<Unit> SClicked { get; }
    }
}