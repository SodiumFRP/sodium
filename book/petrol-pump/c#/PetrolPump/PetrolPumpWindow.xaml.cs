using System;
using System.Collections.Generic;
using System.Linq;
using System.Media;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Resources;
using PetrolPump.Chapter4.Section11;
using PetrolPump.Chapter4.Section4;
using PetrolPump.Chapter4.Section6;
using PetrolPump.Chapter4.Section7;
using PetrolPump.Chapter4.Section8;
using PetrolPump.Chapter4.Section9;
using Sodium;
using Stream = System.IO.Stream;

namespace PetrolPump
{
    public partial class PetrolPumpWindow : IDisposable
    {
        private Implementation implementation;

        public PetrolPumpWindow()
        {
            this.InitializeComponent();

            Transaction.RunVoid(() => { this.implementation = new Implementation(this); });
        }

        public void Dispose()
        {
            using (this.implementation)
            {
            }
        }

        private class Implementation : IDisposable
        {
            private readonly List<IListener> listeners = new List<IListener>();

            public Implementation(PetrolPumpWindow petrolPump)
            {
                SComboBox<IPump> logic = new SComboBox<IPump>(
                    new IPump[]
                    {
                        new LifeCyclePump(),
                        new AccumulatePulsesPump(),
                        new ShowDollarsPump(),
                        new ClearSalePump(),
                        new KeypadPump(),
                        new PresetAmountPump()
                    },
                    p => p.GetType().FullName);
                petrolPump.LogicComboBoxPlaceholder.Children.Add(logic);

                STextField textPrice1 = new STextField("2.149") { Width = 100 };
                petrolPump.Price1Placeholder.Children.Add(textPrice1);

                STextField textPrice2 = new STextField("2.341") { Width = 100 };
                petrolPump.Price2Placeholder.Children.Add(textPrice2);

                STextField textPrice3 = new STextField("1.499") { Width = 100 };
                petrolPump.Price3Placeholder.Children.Add(textPrice3);

                Func<string, double> parseDoubleSafe = s =>
                {
                    double n;
                    if (double.TryParse(s, out n))
                    {
                        return n;
                    }

                    return 0.0;
                };

                StreamSink<Key> sKey = new StreamSink<Key>();
                Dictionary<Key, FrameworkElement> containersByKey = new Dictionary<Key, FrameworkElement>
                {
                    { Key.One, petrolPump.Keypad1Button },
                    { Key.Two, petrolPump.Keypad2Button },
                    { Key.Three, petrolPump.Keypad3Button },
                    { Key.Four, petrolPump.Keypad4Button },
                    { Key.Five, petrolPump.Keypad5Button },
                    { Key.Six, petrolPump.Keypad6Button },
                    { Key.Seven, petrolPump.Keypad7Button },
                    { Key.Eight, petrolPump.Keypad8Button },
                    { Key.Nine, petrolPump.Keypad9Button },
                    { Key.Zero, petrolPump.Keypad0Button },
                    { Key.Clear, petrolPump.KeypadClearButton }
                };
                foreach (KeyValuePair<Key, FrameworkElement> containerAndKey in containersByKey)
                {
                    containerAndKey.Value.MouseDown += async (sender, args) =>
                    {
                        if (args.LeftButton == MouseButtonState.Pressed)
                        {
                            await Task.Run(() => sKey.Send(containerAndKey.Key));
                        }
                    };
                }

                CellLoop<UpDown> nozzle1 = new CellLoop<UpDown>();
                CellLoop<UpDown> nozzle2 = new CellLoop<UpDown>();
                CellLoop<UpDown> nozzle3 = new CellLoop<UpDown>();

                Cell<double> calibration = Cell.Constant(0.001);
                Cell<double> price1 = textPrice1.Text.Map(parseDoubleSafe);
                Cell<double> price2 = textPrice2.Text.Map(parseDoubleSafe);
                Cell<double> price3 = textPrice3.Text.Map(parseDoubleSafe);
                CellSink<Stream<Unit>> csClearSale = new CellSink<Stream<Unit>>(Sodium.Stream.Never<Unit>());
                Stream<Unit> sClearSale = csClearSale.SwitchS();

                StreamSink<int> sFuelPulses = new StreamSink<int>();
                Cell<Outputs> outputs = logic.SelectedItem.Map(
                    pump => pump.Create(new Inputs(
                        Operational.Updates(nozzle1),
                        Operational.Updates(nozzle2),
                        Operational.Updates(nozzle3),
                        sKey,
                        sFuelPulses,
                        calibration,
                        price1,
                        price2,
                        price3,
                        sClearSale)));

                Cell<Delivery> delivery = outputs.Map(o => o.Delivery).SwitchC();
                Cell<string> presetLcd = outputs.Map(o => o.PresetLcd).SwitchC();
                Cell<string> saleCostLcd = outputs.Map(o => o.SaleCostLcd).SwitchC();
                Cell<string> saleQuantityLcd = outputs.Map(o => o.SaleQuantityLcd).SwitchC();
                Cell<string> priceLcd1 = outputs.Map(o => o.PriceLcd1).SwitchC();
                Cell<string> priceLcd2 = outputs.Map(o => o.PriceLcd2).SwitchC();
                Cell<string> priceLcd3 = outputs.Map(o => o.PriceLcd3).SwitchC();
                Stream<Unit> sBeep = outputs.Map(o => o.SBeep).SwitchS();
                Stream<Sale> sSaleComplete = outputs.Map(o => o.SSaleComplete).SwitchS();

                SoundPlayer beepPlayer = new SoundPlayer(GetResourceStream(@"sounds\beep.wav"));
                this.listeners.Add(sBeep.Listen(_ => new Thread(() => beepPlayer.PlaySync()) { IsBackground = true }.Start()));

                SoundPlayer fastRumblePlayer = new SoundPlayer(GetResourceStream(@"sounds\fast.wav"));
                Action stopFast = () => { };
                Action playFast = () =>
                {
                    ManualResetEvent mre = new ManualResetEvent(false);
                    new Thread(() =>
                    {
                        fastRumblePlayer.PlayLooping();
                        mre.WaitOne();
                        fastRumblePlayer.Stop();
                    })
                    { IsBackground = true }.Start();
                    stopFast = () => mre.Set();
                };

                SoundPlayer slowRumblePlayer = new SoundPlayer(GetResourceStream(@"sounds\slow.wav"));
                Action stopSlow = () => { };
                Action playSlow = () =>
                {
                    ManualResetEvent mre = new ManualResetEvent(false);
                    new Thread(() =>
                    {
                        slowRumblePlayer.PlayLooping();
                        mre.WaitOne();
                        slowRumblePlayer.Stop();
                    })
                    { IsBackground = true }.Start();
                    stopSlow = () => mre.Set();
                };

                this.listeners.Add(delivery.Changes().Listen(d =>
                {
                    petrolPump.Dispatcher.InvokeIfNecessary(() =>
                    {
                        if (d == Delivery.Fast1 || d == Delivery.Fast2 || d == Delivery.Fast3)
                        {
                            playFast();
                        }
                        else
                        {
                            stopFast();
                        }

                        if (d == Delivery.Slow1 || d == Delivery.Slow2 || d == Delivery.Slow3)
                        {
                            playSlow();
                        }
                        else
                        {
                            stopSlow();
                        }
                    });
                }));

                TextBlock presetLcdTextBlock = new TextBlock();
                petrolPump.PresetPlaceholder.Children.Add(presetLcdTextBlock);
                this.listeners.Add(presetLcd.Listen(t => petrolPump.Dispatcher.InvokeIfNecessary(() => presetLcdTextBlock.Text = t)));

                TextBlock saleCostTextBlock = new TextBlock();
                petrolPump.DollarsPlaceholder.Children.Add(saleCostTextBlock);
                this.listeners.Add(saleCostLcd.Listen(t => petrolPump.Dispatcher.InvokeIfNecessary(() => saleCostTextBlock.Text = t)));

                TextBlock saleQuantityLcdTextBlock = new TextBlock();
                petrolPump.LitersPlaceholder.Children.Add(saleQuantityLcdTextBlock);
                this.listeners.Add(saleQuantityLcd.Listen(t => petrolPump.Dispatcher.InvokeIfNecessary(() => saleQuantityLcdTextBlock.Text = t)));

                TextBlock priceLcd1TextBlock = new TextBlock();
                petrolPump.Fuel1Placeholder.Children.Add(priceLcd1TextBlock);
                this.listeners.Add(priceLcd1.Listen(t => petrolPump.Dispatcher.InvokeIfNecessary(() => priceLcd1TextBlock.Text = t)));

                TextBlock priceLcd2TextBlock = new TextBlock();
                petrolPump.Fuel2Placeholder.Children.Add(priceLcd2TextBlock);
                this.listeners.Add(priceLcd2.Listen(t => petrolPump.Dispatcher.InvokeIfNecessary(() => priceLcd2TextBlock.Text = t)));

                TextBlock priceLcd3TextBlock = new TextBlock();
                petrolPump.Fuel3Placeholder.Children.Add(priceLcd3TextBlock);
                this.listeners.Add(priceLcd3.Listen(t => petrolPump.Dispatcher.InvokeIfNecessary(() => priceLcd3TextBlock.Text = t)));

                Dictionary<CellLoop<UpDown>, Image> nozzles = new Dictionary<CellLoop<UpDown>, Image>
                {
                    { nozzle1, petrolPump.Nozzle1Image },
                    { nozzle2, petrolPump.Nozzle2Image },
                    { nozzle3, petrolPump.Nozzle3Image }
                };
                this.listeners.AddRange(nozzles.Select(nozzle => nozzle.Key.Listen(p => petrolPump.Dispatcher.InvokeIfNecessary(() => nozzle.Value.Margin = p == UpDown.Up ? new Thickness(0, 0, 0, 0) : new Thickness(0, 30, 0, 0)))));

                foreach (KeyValuePair<CellLoop<UpDown>, Image> nozzle in nozzles)
                {
                    StreamSink<Unit> nozzleClicks = new StreamSink<Unit>();
                    nozzle.Value.MouseDown += async (sender, args) =>
                    {
                        if (args.LeftButton == MouseButtonState.Pressed)
                        {
                            await Task.Run(() => nozzleClicks.Send(Unit.Value));
                        }
                    };
                    nozzle.Key.Loop(nozzleClicks.Snapshot(nozzle.Key, (_, n) => n == UpDown.Down ? UpDown.Up : UpDown.Down).Hold(UpDown.Down));
                }

                this.listeners.Add(sSaleComplete.Listen(sale =>
                {
                    //TODO: show dialog
                }));

                Task.Run(async () =>
                {
                    while (true)
                    {
                        Transaction.RunVoid(() =>
                        {
                            switch (delivery.Sample())
                            {
                                case Delivery.Fast1:
                                case Delivery.Fast2:
                                case Delivery.Fast3:
                                    sFuelPulses.Send(40);
                                    break;
                                case Delivery.Slow1:
                                case Delivery.Slow2:
                                case Delivery.Slow3:
                                    sFuelPulses.Send(2);
                                    break;
                            }
                        });

                        await Task.Delay(200).ConfigureAwait(false);
                    }
                    // ReSharper disable once FunctionNeverReturns
                });
            }

            public void Dispose()
            {
                foreach (IListener l in this.listeners)
                {
                    using (l)
                    {
                    }
                }
            }

            private static Stream GetResourceStream(string path)
            {
                StreamResourceInfo r = Application.GetResourceStream(new Uri(path, UriKind.Relative));

                if (r == null)
                {
                    throw new InvalidOperationException("Could not find " + path + " resource.");
                }

                return r.Stream;
            }
        }
    }

    public enum Key
    {
        One,
        Two,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine,
        Zero,
        Clear
    }
}