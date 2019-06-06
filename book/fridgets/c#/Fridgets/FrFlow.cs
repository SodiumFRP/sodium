using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using Sodium.Frp;
using Sodium.Functional;

namespace Fridgets
{
    public class FrFlow : Fridget
    {
        public FrFlow(Orientation dir, IEnumerable<Fridget> fridgets)
            : base((size, sMouse, sKey, focus, idSupply) =>
            {
                Cell<Size> desiredSize = Cell.Constant(new Size(0, 0));
                Cell<DrawableDelegate> drawable = Cell.Constant(new DrawableDelegate(d => { }));
                Stream<long> sChangeFocus = Stream.Never<long>();
                foreach (Fridget fridget in fridgets)
                {
                    CellLoop<Maybe<Size>> childSz = new CellLoop<Maybe<Size>>();
                    Output fo = new FrTranslate(fridget,
                        dir == Orientation.Horizontal
                            ? desiredSize.Map(dsz => new Point(dsz.Width, 0))
                            : desiredSize.Map(dsz => new Point(0, dsz.Height)))
                        .Reify(childSz, sMouse, sKey, focus,
                            idSupply.Child1());
                    idSupply = idSupply.Child2();
                    childSz.Loop(
                        size.Lift(fo.DesiredSize, (mSize, foDsz) =>
                            mSize.Map(s => dir == Orientation.Horizontal
                                ? new Size(foDsz.Width, s.Height)
                                : new Size(s.Width, foDsz.Height))));
                    Size LiftDesiredSizeHorizontal(Size dsz, Size foDsz) => new Size(dsz.Width + foDsz.Width, dsz.Height > foDsz.Height ? dsz.Height : foDsz.Height);
                    Size LiftDesiredSizeVertical(Size dsz, Size foDsz) => new Size(dsz.Width > foDsz.Width ? dsz.Width : foDsz.Width, dsz.Height + foDsz.Height);
                    desiredSize = desiredSize.Lift(fo.DesiredSize, dir == Orientation.Horizontal ? LiftDesiredSizeHorizontal : (Func<Size, Size, Size>)LiftDesiredSizeVertical);
                    drawable = drawable.Lift(fo.Drawable, (drA, drB) => drA.Append(drB));
                    sChangeFocus = sChangeFocus.OrElse(fo.SChangeFocus);
                }
                return new Output(drawable, desiredSize, sChangeFocus);
            })
        {
        }
    }
}