using System.Windows;
using System.Windows.Media;

namespace Shared
{
    public delegate void DrawableDelegate(DrawingContext drawingContext, double height, Point orig, double scale);
}