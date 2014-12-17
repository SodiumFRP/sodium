using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Schema;

namespace Sodium
{
  static class Extensions
  {
    public static IList<T> Clone<T>(this IEnumerable<T> listToClone) where T : ICloneable
    {
      return listToClone.Select(item => (T)item.Clone()).ToList();
    }

    public static bool IsEmpty<T>(this IEnumerable<T> source)
    {
      return !source.Any();
    }
  }

  public class Optional<T>
  {
    public bool IsPresent { get; private set; }
    private readonly T value;
    public T Get()
    {
      if (IsPresent)
        return value;
      throw new InvalidOperationException();
    }

    private Optional()
    {
      IsPresent = false;
    }

    public Optional(T value)
    {
      this.value = value;
      IsPresent = true;
    }

    public static Optional<T> Of(T value)
    {
      return new Optional<T>(value);
    }

    public static Optional<T> Empty()
    {
      return new Optional<T>();
    }

    public static explicit operator T(Optional<T> optional)
    {
      return optional.Get();
    }
    public static implicit operator Optional<T>(T value)
    {
      return new Optional<T>(value);
    }

    public override bool Equals(object obj)
    {
      if (obj is Optional<T>)
        return Equals((Optional<T>)obj);
      return false;
    }

    public override int GetHashCode()
    {
      var hash = IsPresent.GetHashCode();
      if (value != null)
        hash = (hash * 397) ^ value.GetHashCode();
      return hash;
    }

    public bool Equals(Optional<T> other)
    {
      if (IsPresent && other.IsPresent)
        return Equals(value, other.value);
      return IsPresent == other.IsPresent;
    }
  }
}
