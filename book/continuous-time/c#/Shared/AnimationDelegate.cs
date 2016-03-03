using System.Windows;
using Sodium;
using Sodium.Time;

namespace Shared
{
    public delegate Cell<DrawableDelegate> AnimationDelegate(TimerSystem<double> sys, Point extents);
}