using System;
using System.Windows;
using System.Windows.Input;

namespace Fridgets
{
    public class MouseEvent
    {
        private readonly Func<Point> getPosition;

        public MouseEvent(MouseEventArgs args, Func<Point> getPosition)
        {
            this.Args = args;
            this.getPosition = getPosition;
        }

        public MouseEventArgs Args { get; }

        public Point GetPosition()
        {
            return this.getPosition();
        }
    }
}