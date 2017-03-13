using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using Sodium;

namespace Fridgets
{
    public class FrFlow : Fridget
    {
        public FrFlow(Orientation dir, IEnumerable<Fridget> fridgets)
            : base((size, sMouse, sKey, focus, idSupply) =>
            {
                DiscreteCell<Size> desiredSize = DiscreteCell.Constant(new Size(0, 0));
                DiscreteCell<DrawableDelegate> drawable = DiscreteCell.Constant(new DrawableDelegate(d => { }));
                Stream<long> sChangeFocus = Stream.Never<long>();
                foreach (Fridget fridget in fridgets)
                {
                    DiscreteCellLoop<IMaybe<Size>> childSz = new DiscreteCellLoop<IMaybe<Size>>();
                    Output fo = new FrTranslate(fridget,
                        dir == Orientation.Horizontal
                            ? desiredSize.Map(dsz => new Point(dsz.Width, 0))
                            : desiredSize.Map(dsz => new Point(0, dsz.Height)))
                        .Reify(childSz, sMouse, sKey, focus,
                            idSupply.Child1());
                    idSupply = idSupply.Child2();
                    childSz.Loop(
                        size.Lift(fo.DesiredSize, (mSize, foDsz) =>
                            mSize.Match(s => Maybe.Just(dir == Orientation.Horizontal
                                ? new Size(foDsz.Width, s.Height)
                                : new Size(s.Width, foDsz.Height)),
                                Maybe.Nothing<Size>)));
                    Func<Size, Size, Size> liftDesiredSizeHorizontal = (dsz, foDsz) => new Size(
                        dsz.Width + foDsz.Width,
                        dsz.Height > foDsz.Height ? dsz.Height : foDsz.Height);
                    Func<Size, Size, Size> liftDesiredSizeVertical = (dsz, foDsz) => new Size(
                        dsz.Width > foDsz.Width ? dsz.Width : foDsz.Width,
                        dsz.Height + foDsz.Height);
                    desiredSize = desiredSize.Lift(fo.DesiredSize, dir == Orientation.Horizontal ? liftDesiredSizeHorizontal : liftDesiredSizeVertical);
                    drawable = drawable.Lift(fo.Drawable, (drA, drB) => drA.Append(drB));
                    sChangeFocus = sChangeFocus.OrElse(fo.SChangeFocus);
                }
                return new Output(drawable, desiredSize, sChangeFocus);
            })
        {
        }
    }
}