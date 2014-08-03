using System;

namespace Sodium
{
  public class Listener
  {
    public Listener()
    {
      Unlisten = () => { };
    }
    public Action Unlisten { get; protected set; }

    public Listener Append(Listener two)
    {
      Listener one = this;
      return new Listener { Unlisten = () => { one.Unlisten(); two.Unlisten(); } };
    }
  }
}
