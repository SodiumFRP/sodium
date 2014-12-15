using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Sodium;

namespace PetrolPump.Pump
{
  public class Outputs
  {
    private Outputs(
            Behavior<Delivery> delivery,
            Behavior<String> presetLcd,
            Behavior<String> saleCostLcd,
            Behavior<String> saleQuantityLcd,
            Behavior<String> priceLcd1,
            Behavior<String> priceLcd2,
            Behavior<String> priceLcd3,
            Event<Unit> eBeep,
            Event<Sale> eSaleComplete) 
    {
        Delivery = delivery;
        PresetLcd = presetLcd;
        SaleCostLcd = saleCostLcd;
        SaleQuantityLcd = saleQuantityLcd;
        PriceLcd1 = priceLcd1;
        PriceLcd2 = priceLcd2;
        PriceLcd3 = priceLcd3;
        EBeep = eBeep;
        ESaleComplete = eSaleComplete;
    }

    public Outputs() 
    {
        this.Delivery = new Behavior<Delivery>(Pump.Delivery.OFF);
        this.PresetLcd = new Behavior<String>("");
        this.SaleCostLcd = new Behavior<String>("");
        this.SaleQuantityLcd = new Behavior<String>("");
        this.PriceLcd1 = new Behavior<String>("");
        this.PriceLcd2 = new Behavior<String>("");
        this.PriceLcd3 = new Behavior<String>("");
        this.EBeep = new Event<Unit>();
        this.ESaleComplete = new Event<Sale>();
    }

    public readonly Behavior<Delivery> Delivery;
    public readonly Behavior<String> PresetLcd;
    public readonly Behavior<String> SaleCostLcd;
    public readonly Behavior<String> SaleQuantityLcd;
    public readonly Behavior<String> PriceLcd1;
    public readonly Behavior<String> PriceLcd2;
    public readonly Behavior<String> PriceLcd3;
    public readonly Event<Unit> EBeep;
    public readonly Event<Sale> ESaleComplete;

    public Outputs SetDelivery(Behavior<Delivery> delivery) 
    {
        return new Outputs(delivery, PresetLcd, SaleCostLcd,
                SaleQuantityLcd, PriceLcd1, PriceLcd2, PriceLcd3, EBeep,
                ESaleComplete);
    }
    public Outputs SetPresetLcd(Behavior<String> presetLcd) 
    {
        return new Outputs(Delivery, presetLcd, SaleCostLcd,
                SaleQuantityLcd, PriceLcd1, PriceLcd2, PriceLcd3, EBeep,
                ESaleComplete);
    }
    public Outputs SetSaleCostLcd(Behavior<String> saleCostLcd) 
    {
        return new Outputs(Delivery, PresetLcd, saleCostLcd,
                SaleQuantityLcd, PriceLcd1, PriceLcd2, PriceLcd3, EBeep,
                ESaleComplete);
    }
    public Outputs SetSaleQuantityLcd(Behavior<String> saleQuantityLcd) 
    {
        return new Outputs(Delivery, PresetLcd, SaleCostLcd,
                saleQuantityLcd, PriceLcd1, PriceLcd2, PriceLcd3, EBeep,
                ESaleComplete);
    }
    public Outputs SetPriceLcd1(Behavior<String> priceLcd1) 
    {
        return new Outputs(Delivery, PresetLcd, SaleCostLcd,
                SaleQuantityLcd, priceLcd1, PriceLcd2, PriceLcd3, EBeep,
                ESaleComplete);
    }
    public Outputs SetPriceLcd2(Behavior<String> priceLcd2) 
    {
        return new Outputs(Delivery, PresetLcd, SaleCostLcd,
                SaleQuantityLcd, PriceLcd1, priceLcd2, PriceLcd3, EBeep,
                ESaleComplete);
    }
    public Outputs SetPriceLcd3(Behavior<String> priceLcd3) 
    {
        return new Outputs(Delivery, PresetLcd, SaleCostLcd,
                SaleQuantityLcd, PriceLcd1, PriceLcd2, priceLcd3, EBeep,
                ESaleComplete);
    }
    public Outputs SetBeep(Event<Unit> eBeep) 
    {
        return new Outputs(Delivery, PresetLcd, SaleCostLcd,
                SaleQuantityLcd, PriceLcd1, PriceLcd2, PriceLcd3, eBeep,
                ESaleComplete);
    }
    public Outputs SetSaleComplete(Event<Sale> eSaleComplete) 
    {
        return new Outputs(Delivery, PresetLcd, SaleCostLcd,
                SaleQuantityLcd, PriceLcd1, PriceLcd2, PriceLcd3, EBeep,
                eSaleComplete);
    }
  }

}




