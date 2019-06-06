namespace PetrolPump

open Sodium.Frp

type UpDown = Up | Down

type Fuel = One | Two | Three

type FillState = Idle | Filling | SaleComplete

type Key = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero | Clear

type Delivery = Off | Slow1 | Fast1 | Slow2 | Fast2 | Slow3 | Fast3