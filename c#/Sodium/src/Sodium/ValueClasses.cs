using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sodium.src.Sodium
{
  public class Boolean
  {
    bool value = false;

    public Boolean(bool value)
    {
      this.value = value;
    }

    public static implicit operator Boolean(bool value)
    {
      return new Boolean(value);
    }

    public static implicit operator bool(Boolean boolean)
    {
      return boolean.value;
    }

    public override string ToString()
    {
      return value.ToString();
    }
  }

  public class Integer
  {
    int value = 0;

    public Integer(int value)
    {
      this.value = value;
    }

    public static implicit operator Integer(int value)
    {
      return new Integer(value);
    }

    public static implicit operator int(Integer integer)
    {
      return integer.value;
    }

    public static int operator +(Integer one, Integer two)
    {
      return one.value + two.value;
    }

    public static Integer operator +(int one, Integer two)
    {
      return new Integer(one + two);
    }

    public static int operator -(Integer one, Integer two)
    {
      return one.value - two.value;
    }

    public static Integer operator -(int one, Integer two)
    {
      return new Integer(one - two);
    }

    public override string ToString()
    {
      return value.ToString();
    }
  }

  public class Long
  {
    long value = 0;

    public Long(long value)
    {
      this.value = value;
    }

    public static implicit operator Long(long value)
    {
      return new Long(value);
    }

    public static implicit operator long(Long num)
    {
      return num.value;
    }

    public static long operator +(Long one, Long two)
    {
      return one.value + two.value;
    }

    public static Long operator +(long one, Long two)
    {
      return new Long(one + two);
    }

    public static long operator -(Long one, Long two)
    {
      return one.value - two.value;
    }

    public static Long operator -(long one, Long two)
    {
      return new Long(one - two);
    }

    public override string ToString()
    {
      return value.ToString();
    }
  }

  public class Character
  {
    char value = '\0';

    public Character(char value)
    {
      this.value = value;
    }

    public static implicit operator Character(char value)
    {
      return new Character(value);
    }

    public static implicit operator int(Character integer)
    {
      return integer.value;
    }

    public static int operator +(Character one, Character two)
    {
      return one.value + two.value;
    }

    public static Character operator +(char one, Character two)
    {
      return new Character((char)(one + two));
    }

    public static int operator -(Character one, Character two)
    {
      return one.value - two.value;
    }

    public static Character operator -(char one, Character two)
    {
      return new Character((char)(one - two));
    }

    public override string ToString()
    {
      return value.ToString();
    }
  }
}
