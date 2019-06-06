using PetrolPump.Chapter4.Section7;
using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump.Chapter4.Section11
{
    internal class Preset
    {
        public Cell<Delivery> Delivery { get; }
        public Cell<bool> KeypadActive { get; }

        private enum Speed
        {
            Fast,
            Slow,
            Stopped
        };

        public Preset(Cell<int> presetDollars, Fill fi, Cell<Maybe<Fuel>> fuelFlowing)
        {
            Cell<Speed> speed = presetDollars.Lift(
                fi.Price, fi.DollarsDelivered, fi.LitersDelivered,
                (presetDollarsLocal, price, dollarsDelivered, litersDelivered) =>
                {
                    if (presetDollarsLocal == 0)
                    {
                        return Speed.Fast;
                    }

                    if (dollarsDelivered >= presetDollarsLocal)
                    {
                        return Speed.Stopped;
                    }

                    double slowLiters = presetDollarsLocal / price - 0.10;
                    return litersDelivered >= slowLiters ? Speed.Slow : Speed.Fast;
                });
            this.Delivery = fuelFlowing.Lift(speed,
                (m, speedLocal) =>
                    speedLocal == Speed.Fast ? (
                        m.Equals(Maybe.Some(Fuel.One)) ? PetrolPump.Delivery.Fast1 :
                            m.Equals(Maybe.Some(Fuel.Two)) ? PetrolPump.Delivery.Fast2 :
                                m.Equals(Maybe.Some(Fuel.Three)) ? PetrolPump.Delivery.Fast3 : PetrolPump.Delivery.Off
                        ) :
                        speedLocal == Speed.Slow ? (
                            m.Equals(Maybe.Some(Fuel.One)) ? PetrolPump.Delivery.Slow1 :
                                m.Equals(Maybe.Some(Fuel.Two)) ? PetrolPump.Delivery.Slow2 :
                                    m.Equals(Maybe.Some(Fuel.Three)) ? PetrolPump.Delivery.Slow3 : PetrolPump.Delivery.Off
                            ) : PetrolPump.Delivery.Off);
            this.KeypadActive = fuelFlowing.Lift(speed, (m, speedLocal) => m.Match(_ => speedLocal == Speed.Fast, () => true));
        }
    }
}