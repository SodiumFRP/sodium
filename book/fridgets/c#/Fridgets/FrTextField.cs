using System;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using Sodium.Frp;
using Sodium.Functional;

namespace Fridgets
{
    public class FrTextField : Fridget
    {
        public FrTextField(string initText)
            : this(initText, new CellLoop<string>())
        {
        }

        private FrTextField(string initText, CellLoop<string> text)
            : base((size, sMouse, sKey, focus, idSupply) =>
            {
                Stream<double> sPressed = sMouse.Snapshot(size, (e, mSize) =>
                    mSize.Bind(
                        s =>
                        {
                            MouseButtonEventArgs b = e.Args as MouseButtonEventArgs;
                            Point p = e.GetPosition();
                            return b != null && b.ChangedButton == MouseButton.Left && b.ButtonState == MouseButtonState.Pressed
                                   && p.X >= 2 && p.X < s.Width - 2 && p.Y >= 2 && p.Y < s.Height - 2
                                ? Maybe.Some(p.X - 2)
                                : Maybe.None;
                        })).FilterMaybe();
                CellLoop<int> x = new CellLoop<int>();
                long myId = idSupply.Get();
                Cell<bool> haveFocus = focus.Map(fId => fId == myId);
                Typeface typeface = new Typeface(new FontFamily("Helvetica"), FontStyles.Normal, FontWeights.Normal, FontStretches.Normal);
                Stream<TextUpdate> sTextUpdate = sKey.Gate(haveFocus).Snapshot(text, x, (key, txt, xValue) =>
                 {
                     if (key is BackspaceKeyEvent)
                     {
                         return xValue > 0 ? Maybe.Some(new TextUpdate(
                             txt.Substring(0, xValue - 1) +
                             txt.Substring(xValue),
                             xValue - 1)) : Maybe.None;
                     }

                     if (!(key is StringKeyEvent stringKey))
                     {
                         throw new InvalidOperationException("Unexpected type encountered for " + typeof(KeyEvent).FullName + ": " + key.GetType().FullName + ".");
                     }

                     string keyString = stringKey.String;
                     return keyString == "\b" ? Maybe.None :
                         Maybe.Some(new TextUpdate(
                             txt.Substring(0, xValue) +
                             keyString +
                             txt.Substring(xValue),
                             xValue + 1));
                 }).FilterMaybe();
                x.Loop(sPressed.Snapshot(text,
                    (xCoord, txt) =>
                    {
                        for (int i = 1; i <= txt.Length; i++)
                        {
                            if (xCoord < FontUtilities.MeasureString(txt.Substring(0, i), typeface, 13).Width)
                            {
                                return i - 1;
                            }
                        }
                        return txt.Length;
                    })
                    .OrElse(sTextUpdate.Map(tu => tu.NewX))
                    .Hold(0));
                text.Loop(sTextUpdate.Map(tu => tu.Txt).Hold(initText));
                Cell<Size> desiredSize = text.Map(txt =>
                {
                    Size s = FontUtilities.MeasureString(txt, typeface, 13);
                    return new Size(s.Width + 14, s.Height + 10);
                });
                return new Output(
                    text.Lift(
                        x, haveFocus, size,
                        (txt, xValue, haveFocusValue, mSize) => new DrawableDelegate(d =>
                        {
                            mSize.MatchSome(
                                sz =>
                                {
                                    d.DrawRectangle(Brushes.White, new Pen(Brushes.Black, 1), new Rect(new Point(2, 2), new Size(sz.Width - 5, sz.Height - 5)));
                                    FormattedText t = FontUtilities.GetStandardFormattedText(txt, typeface, 13, Brushes.Black);
                                    FormattedText tCursor = FontUtilities.GetStandardFormattedText(txt.Substring(0, xValue), typeface, 13, Brushes.Black);
                                    d.DrawText(t, new Point(4, (sz.Height - t.Height) / 2));
                                    if (haveFocusValue)
                                    {
                                        double cursorX = tCursor.Width;
                                        d.DrawLine(new Pen(Brushes.Red, 1), new Point(4 + cursorX, 4), new Point(4 + cursorX, sz.Height - 5));
                                    }
                                });
                        })),
                    desiredSize,
                    sPressed.Map(_ => myId));
            })
        {
            this.Text = text;
        }

        public Cell<string> Text { get; }

        private class TextUpdate
        {
            public TextUpdate(string txt, int newX)
            {
                this.Txt = txt;
                this.NewX = newX;
            }

            public string Txt { get; }
            public int NewX { get; }
        }
    }
}