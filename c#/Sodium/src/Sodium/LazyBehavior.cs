using System;

namespace Sodium
{
  public class LazyBehavior<TA> : Behavior<TA> where TA : class
  {

    public LazyBehavior(Event<TA> @event, Func<TA> lazyInitValue)
      : base(@event, null, resetInitValue: true)
    {
      LazyInitValue = lazyInitValue;
    }

    public override TA SampleNoTrans()
    {
      if (!_isEventValueSet)
      {
        EventValue = LazyInitValue();
        LazyInitValue = null;
      }
      return EventValue;
    }
  }
}

