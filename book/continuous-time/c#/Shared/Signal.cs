using System;
using Sodium.Frp;
using Sodium.Functional;

namespace Shared
{
    public class Signal
    {
        public static readonly double Quantum = 0.000001;

        public Signal(double t0, double a, double b, double c)
        {
            this.T0 = t0;
            this.A = a;
            this.B = b;
            this.C = c;
        }

        public double T0 { get; }
        public double A { get; }
        public double B { get; }
        public double C { get; }

        private static bool IsCloseTo(double x, double y)
        {
            return Math.Abs(x - y) < Quantum;
        }

        public double ValueAt(double t)
        {
            double x = t - this.T0;
            return this.A * x * x + this.B * x + this.C;
        }

        public Maybe<double> When(double x)
        {
            double c = this.C - x;
            if (IsCloseTo(this.A, 0))
            {
                double t = -c / this.B;
                return t >= Quantum ? Maybe.Some(t + this.T0) : Maybe.None;
            }

            // ReSharper disable once InconsistentNaming
            double b24ac = Math.Sqrt(this.B * this.B - 4 * this.A * c);
            double t1 = (-this.B + b24ac) / (2 * this.A);
            double t2 = (-this.B - b24ac) / (2 * this.A);
            return t1 >= Quantum
                ? t2 >= Quantum
                    ? Maybe.Some((t1 < t2 ? t1 : t2) + this.T0)
                    : Maybe.Some(t1 + this.T0)
                : t2 >= Quantum
                    ? Maybe.Some(t2 + this.T0)
                    : Maybe.None;
        }

        public Signal Integrate(double initial)
        {
            if (!IsCloseTo(this.A, 0.0))
            {
                throw new Exception("Signal can't handle x^3");
            }

            return new Signal(this.T0, this.B / 2, this.C, initial);
        }

        public static Cell<Signal> Integrate(Cell<Signal> sig, double initial)
        {
            Stream<Signal> sSig = sig.Updates();
            return sSig.Accum(sig.Sample().Integrate(initial), (n, o) => n.Integrate(o.ValueAt(n.T0)));
        }
    }
}