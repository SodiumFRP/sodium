using System.Windows;
using SodiumFRP;
using SodiumFRP.Time;

namespace Shared
{
    public delegate Behavior<DrawableDelegate> AnimationDelegate(TimerSystem<double> sys, Point extents);
}