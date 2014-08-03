using System;

namespace Sodium
{
  public class Handler<TA>
  {
    public bool Fired { get; set; }
    public Action<TA> Run { get; set; }
  }
}
