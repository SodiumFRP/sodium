using System;

namespace Sodium
{
  public class LazyBehavior<TA> : Behavior<TA>
  {

    public LazyBehavior(Event<TA> @event, Func<TA> lazyInitValue)
      : base(@event, default(TA))
    {
      LazyInitValue = lazyInitValue;
    }

    public override TA SampleNoTrans()
    {
      if (EventValue == null)
      {
        EventValue = LazyInitValue();
        LazyInitValue = null;
      }
      return EventValue;
    }
  }
}

