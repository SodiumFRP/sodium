using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PetrolPump.Pump
{
  public interface IPump
  {
    Outputs Create(Inputs inputs);
  }
}
