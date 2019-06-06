using System;
using System.Windows;
using Sodium.Frp;
using Sodium.Functional;

namespace Fridgets
{
    public abstract class Fridget
    {
        public class Output
        {
            public Output(Cell<DrawableDelegate> drawable, Cell<Size> desiredSize, Stream<long> sChangeFocus)
            {
                this.Drawable = drawable;
                this.DesiredSize = desiredSize;
                this.SChangeFocus = sChangeFocus;
            }

            public Cell<DrawableDelegate> Drawable { get; }
            public Cell<Size> DesiredSize { get; }
            public Stream<long> SChangeFocus { get; }
        }

        private readonly Func<Cell<Maybe<Size>>, Stream<MouseEvent>, Stream<KeyEvent>, Cell<long>, Supply, Output> reify;

        protected Fridget(Func<Cell<Maybe<Size>>, Stream<MouseEvent>, Stream<KeyEvent>, Cell<long>, Supply, Output> reify)
        {
            this.reify = reify;
        }

        public Output Reify(Cell<Maybe<Size>> size, Stream<MouseEvent> sMouse, Stream<KeyEvent> sKey, Cell<long> focus, Supply idSupply)
        {
            return this.reify(size, sMouse, sKey, focus, idSupply);
        }
    }
}