using System;

namespace Sodium
{

  public class BehaviorLoop<TA> : Behavior<TA>
  {
    public BehaviorLoop()
      : base(new EventLoop<TA>(), default(TA))
    {
    }

    public void Loop(Behavior<TA> aOut)
    {
      ((EventLoop<TA>)Event).Loop(aOut.Updates());
      EventValue = aOut.Sample();
    }

    public override TA SampleNoTrans()
    {
      if (EventValue == null)
        throw new Exception("BehaviorLoop sampled before it was looped");
      return EventValue;
    }
  }
}
