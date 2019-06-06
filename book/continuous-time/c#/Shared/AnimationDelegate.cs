using System.Windows;
using Sodium.Frp;
using Sodium.Frp.Time;

namespace Shared
{
    public delegate Behavior<DrawableDelegate> AnimationDelegate(TimerSystem<double> sys, Point extents);
}