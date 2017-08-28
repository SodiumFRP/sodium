using System;
using System.Windows;
using Sodium;

namespace Fridgets
{
    public abstract class Fridget
    {
        public class Output
        {
            public Output(
                DiscreteCell<DrawableDelegate> drawable,
                DiscreteCell<Size> desiredSize,
                Stream<long> sChangeFocus)
            {
                this.Drawable = drawable;
                this.DesiredSize = desiredSize;
                this.SChangeFocus = sChangeFocus;
            }

            public DiscreteCell<DrawableDelegate> Drawable { get; }
            public DiscreteCell<Size> DesiredSize { get; }
            public Stream<long> SChangeFocus { get; }
        }

        private readonly Func<
            DiscreteCell<IMaybe<Size>>, Stream<MouseEvent>,
            Stream<KeyEvent>, DiscreteCell<long>, Supply, Output> reify;

        protected Fridget(Func<
            DiscreteCell<IMaybe<Size>>, Stream<MouseEvent>,
            Stream<KeyEvent>, DiscreteCell<long>, Supply, Output> reify)
        {
            this.reify = reify;
        }

        public Output Reify(
            DiscreteCell<IMaybe<Size>> size,
            Stream<MouseEvent> sMouse, Stream<KeyEvent> sKey,
            DiscreteCell<long> focus, Supply idSupply)
        {
            return this.reify(size, sMouse, sKey, focus, idSupply);
        }
    }
}