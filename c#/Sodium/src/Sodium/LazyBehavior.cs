using System;

namespace Sodium
{
  public class LazyBehavior<TA> : Behavior<TA>
  {

    public LazyBehavior(Event<TA> @event, Func<TA> lazyInitValue)
      : base(@event, default(TA), resetInitValue: true)
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

