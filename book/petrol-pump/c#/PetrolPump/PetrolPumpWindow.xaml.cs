using System;
using System.Collections.Generic;
using System.Linq;
using System.Media;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media.Imaging;
using System.Windows.Resources;
using PetrolPump.Chapter4.Section11;
using PetrolPump.Chapter4.Section4;
using PetrolPump.Chapter4.Section6;
using PetrolPump.Chapter4.Section7;
using PetrolPump.Chapter4.Section8;
using PetrolPump.Chapter4.Section9;
using Sodium.Frp;
using Sodium.Functional;
using Stream = Sodium.Frp.Stream;

namespace PetrolPump
{
    public partial class PetrolPumpWindow : IDisposable
    {
        private readonly string assemblyName;

        private readonly Func<Grid> largeNumberImage0;
        private readonly Func<Grid> smallNumberImage0;
        private readonly Func<Grid> largeNumberImage1;
        private readonly Func<Grid> smallNumberImage1;
        private readonly Func<Grid> largeNumberImage2;
        private readonly Func<Grid> smallNumberImage2;
        private readonly Func<Grid> largeNumberImage3;
        private readonly Func<Grid> smallNumberImage3;
        private readonly Func<Grid> largeNumberImage4;
        private readonly Func<Grid> smallNumberImage4;
        private readonly Func<Grid> largeNumberImage5;
        private readonly Func<Grid> smallNumberImage5;
        private readonly Func<Grid> largeNumberImage6;
        private readonly Func<Grid> smallNumberImage6;
        private readonly Func<Grid> largeNumberImage7;
        private readonly Func<Grid> smallNumberImage7;
        private readonly Func<Grid> largeNumberImage8;
        private readonly Func<Grid> smallNumberImage8;
        private readonly Func<Grid> largeNumberImage9;
        private readonly Func<Grid> smallNumberImage9;
        private readonly Func<Grid> largeDotImage;
        private readonly Func<Grid> smallDotImage;
        private readonly Func<Grid> largeDashImage;
        private readonly Func<Grid> smallDashImage;

        private Implementation implementation;

        public PetrolPumpWindow()
        {
            this.assemblyName = this.GetType().Assembly.FullName;

            this.InitializeComponent();

            Func<Image>[] largeSegments = new Func<Image>[8];
            Func<Image>[] smallSegments = new Func<Image>[8];
            for (int i = 0; i < 8; i++)
            {
                int n = i;

                largeSegments[i] = () =>
                {
                    Image image = new Image();
                    BitmapImage source = new BitmapImage();
                    source.BeginInit();
                    source.UriSource = new Uri("pack://application:,,,/" + this.assemblyName +
                                               ";component/images/large" + n + ".png");
                    source.EndInit();
                    image.Source = source;
                    image.Width = source.PixelWidth;
                    image.Height = source.PixelHeight;
                    return image;
                };

                smallSegments[i] = () =>
                {
                    Image image = new Image();
                    BitmapImage source = new BitmapImage();
                    source.BeginInit();
                    source.UriSource = new Uri("pack://application:,,,/" + this.assemblyName +
                                               ";component/images/small" + n + ".png");
                    source.EndInit();
                    image.Source = source;
                    image.Width = source.PixelWidth;
                    image.Height = source.PixelHeight;
                    return image;
                };
            }

            IReadOnlyList<IReadOnlyList<int>> layouts = new[]
            {
                new[] {0, 1, 2, 4, 5, 6},
                new[] {2, 5},
                new[] {0, 1, 3, 5, 6},
                new[] {0, 2, 3, 5, 6},
                new[] {2, 3, 4, 5},
                new[] {0, 2, 3, 4, 6},
                new[] {0, 1, 2, 3, 4, 6},
                new[] {2, 5, 6},
                new[] {0, 1, 2, 3, 4, 5, 6},
                new[] {2, 3, 4, 5, 6}
            };
            Func<Grid>[] largeNumberImages = new Func<Grid>[10];
            Func<Grid>[] smallNumberImages = new Func<Grid>[10];
            for (int i = 0; i < 10; i++)
            {
                IReadOnlyList<int> layout = layouts[i];

                largeNumberImages[i] = () =>
                {
                    Grid grid = new Grid();
                    foreach (int n in layout)
                    {
                        grid.Children.Add(largeSegments[n]());
                    }

                    return grid;
                };

                smallNumberImages[i] = () =>
                {
                    Grid grid = new Grid();
                    foreach (int n in layout)
                    {
                        grid.Children.Add(smallSegments[n]());
                    }

                    return grid;
                };
            }

            Func<Grid> largeDotImage = () =>
            {
                Grid grid = new Grid();
                grid.Children.Add(largeSegments[7]());
                return grid;
            };

            Func<Grid> smallDotImage = () =>
            {
                Grid grid = new Grid();
                grid.Children.Add(smallSegments[7]());
                return grid;
            };

            Func<Grid> largeDashImage = () =>
            {
                Grid grid = new Grid();
                grid.Children.Add(largeSegments[3]());
                return grid;
            };

            Func<Grid> smallDashImage = () =>
            {
                Grid grid = new Grid();
                grid.Children.Add(smallSegments[3]());
                return grid;
            };

            this.largeNumberImage0 = largeNumberImages[0];
            this.smallNumberImage0 = smallNumberImages[0];
            this.largeNumberImage1 = largeNumberImages[1];
            this.smallNumberImage1 = smallNumberImages[1];
            this.largeNumberImage2 = largeNumberImages[2];
            this.smallNumberImage2 = smallNumberImages[2];
            this.largeNumberImage3 = largeNumberImages[3];
            this.smallNumberImage3 = smallNumberImages[3];
            this.largeNumberImage4 = largeNumberImages[4];
            this.smallNumberImage4 = smallNumberImages[4];
            this.largeNumberImage5 = largeNumberImages[5];
            this.smallNumberImage5 = smallNumberImages[5];
            this.largeNumberImage6 = largeNumberImages[6];
            this.smallNumberImage6 = smallNumberImages[6];
            this.largeNumberImage7 = largeNumberImages[7];
            this.smallNumberImage7 = smallNumberImages[7];
            this.largeNumberImage8 = largeNumberImages[8];
            this.smallNumberImage8 = smallNumberImages[8];
            this.largeNumberImage9 = largeNumberImages[9];
            this.smallNumberImage9 = smallNumberImages[9];
            this.largeDotImage = largeDotImage;
            this.smallDotImage = smallDotImage;
            this.largeDashImage = largeDashImage;
            this.smallDashImage = smallDashImage;

            Transaction.RunVoid(() => { this.implementation = new Implementation(this); });
        }

        private Grid GetImage(char c, bool isLarge)
        {
            switch (c)
            {
                case '-':
                    return isLarge ? this.largeDashImage() : this.smallDashImage();
                case '0':
                    return isLarge ? this.largeNumberImage0() : this.smallNumberImage0();
                case '1':
                    return isLarge ? this.largeNumberImage1() : this.smallNumberImage1();
                case '2':
                    return isLarge ? this.largeNumberImage2() : this.smallNumberImage2();
                case '3':
                    return isLarge ? this.largeNumberImage3() : this.smallNumberImage3();
                case '4':
                    return isLarge ? this.largeNumberImage4() : this.smallNumberImage4();
                case '5':
                    return isLarge ? this.largeNumberImage5() : this.smallNumberImage5();
                case '6':
                    return isLarge ? this.largeNumberImage6() : this.smallNumberImage6();
                case '7':
                    return isLarge ? this.largeNumberImage7() : this.smallNumberImage7();
                case '8':
                    return isLarge ? this.largeNumberImage8() : this.smallNumberImage8();
                case '9':
                    return isLarge ? this.largeNumberImage9() : this.smallNumberImage9();
                case '.':
                    return isLarge ? this.largeDotImage() : this.smallDotImage();
            }

            throw new ArgumentException(@"Character must be a dot, dash, or number between 0 and 9.", nameof(c));
        }

        private void SetLcdDigits(StackPanel placeholder, string text, int maxDigits, bool isLarge)
        {
            placeholder.Children.Clear();
            Func<IEnumerable<Grid>, Grid, IEnumerable<Grid>> defaultAppend = (gg, g) => gg.Concat(new[] {g});
            // ReSharper disable once PossibleMultipleEnumeration
            Func<IEnumerable<Grid>, IEnumerable<Grid>> defaultComplete = g => g;
            var initialState = new
            {
                Append = defaultAppend,
                Grids = new Grid[0].AsEnumerable(),
                Complete = defaultComplete
            };
            foreach (Grid g in text.ToArray().Reverse().Aggregate(initialState, (s, c) =>
            {
                Grid imageGrid = this.GetImage(c, isLarge);
                if (c == '.')
                {
                    Grid container = new Grid();
                    container.Children.Add(imageGrid);
                    return new
                    {
                        Append = (Func<IEnumerable<Grid>, Grid, IEnumerable<Grid>>) ((gg, g) =>
                        {
                            container.Children.Add(g);
                            return s.Append(gg, container);
                        }),
                        s.Grids,
                        Complete = (Func<IEnumerable<Grid>, IEnumerable<Grid>>) (g => g.Concat(new[] {imageGrid}))
                    };
                }

                return new {Append = defaultAppend, Grids = s.Append(s.Grids, imageGrid), Complete = defaultComplete};
            }, s => s.Complete(s.Grids)).Where(i => i != null).Take(maxDigits).Reverse())
            {
                placeholder.Children.Add(g);
            }
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

                STextField textPrice1 = new STextField("2.149") {Width = 100};
                petrolPump.Price1Placeholder.Children.Add(textPrice1);

                STextField textPrice2 = new STextField("2.341") {Width = 100};
                petrolPump.Price2Placeholder.Children.Add(textPrice2);

                STextField textPrice3 = new STextField("1.499") {Width = 100};
                petrolPump.Price3Placeholder.Children.Add(textPrice3);

                double ParseDoubleSafe(string s)
                {
                    double n;
                    if (double.TryParse(s, out n))
                    {
                        return n;
                    }

                    return 0.0;
                }

                StreamSink<Key> sKey = Stream.CreateSink<Key>();
                Dictionary<Key, FrameworkElement> containersByKey = new Dictionary<Key, FrameworkElement>
                {
                    {Key.One, petrolPump.Keypad1Button},
                    {Key.Two, petrolPump.Keypad2Button},
                    {Key.Three, petrolPump.Keypad3Button},
                    {Key.Four, petrolPump.Keypad4Button},
                    {Key.Five, petrolPump.Keypad5Button},
                    {Key.Six, petrolPump.Keypad6Button},
                    {Key.Seven, petrolPump.Keypad7Button},
                    {Key.Eight, petrolPump.Keypad8Button},
                    {Key.Nine, petrolPump.Keypad9Button},
                    {Key.Zero, petrolPump.Keypad0Button},
                    {Key.Clear, petrolPump.KeypadClearButton}
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
                Cell<double> price2 = textPrice2.Text.Map(ParseDoubleSafe);
                Cell<double> price3 = textPrice3.Text.Map(ParseDoubleSafe);
                Cell<double> price1 = textPrice1.Text.Map(ParseDoubleSafe);
                CellSink<Stream<Unit>> csClearSale = Cell.CreateSink(Stream.Never<Unit>());
                Stream<Unit> sClearSale = csClearSale.SwitchS();

                StreamSink<int> sFuelPulses = Stream.CreateSink<int>();
                Cell<Outputs> outputs = logic.SelectedItem.Map(
                    pump => pump.Create(new Inputs(
                        nozzle1.Updates(),
                        nozzle2.Updates(),
                        nozzle3.Updates(),
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
                this.listeners.Add(sBeep.Listen(_ =>
                    new Thread(() => beepPlayer.PlaySync()) {IsBackground = true}.Start()));

                SoundPlayer fastRumblePlayer = new SoundPlayer(GetResourceStream(@"sounds\fast.wav"));
                Action stopFast = () => { };

                void PlayFast()
                {
                    ManualResetEvent mre = new ManualResetEvent(false);
                    new Thread(() =>
                    {
                        fastRumblePlayer.PlayLooping();
                        mre.WaitOne();
                        fastRumblePlayer.Stop();
                    }) {IsBackground = true}.Start();
                    stopFast = () =>
                    {
                        mre.Set();
                        stopFast = () => { };
                    };
                }

                SoundPlayer slowRumblePlayer = new SoundPlayer(GetResourceStream(@"sounds\slow.wav"));
                Action stopSlow = () => { };

                void PlaySlow()
                {
                    ManualResetEvent mre = new ManualResetEvent(false);
                    new Thread(() =>
                    {
                        slowRumblePlayer.PlayLooping();
                        mre.WaitOne();
                        slowRumblePlayer.Stop();
                    }) {IsBackground = true}.Start();
                    stopSlow = () =>
                    {
                        mre.Set();
                        stopSlow = () => { };
                    };
                }

                this.listeners.Add(delivery.Changes().Listen(d =>
                {
                    petrolPump.Dispatcher.InvokeAsync(() =>
                    {
                        if (d == Delivery.Fast1 || d == Delivery.Fast2 || d == Delivery.Fast3)
                        {
                            PlayFast();
                        }
                        else
                        {
                            stopFast();
                        }

                        if (d == Delivery.Slow1 || d == Delivery.Slow2 || d == Delivery.Slow3)
                        {
                            PlaySlow();
                        }
                        else
                        {
                            stopSlow();
                        }
                    });
                }));

                StackPanel presetLcdStackPanel = new StackPanel {Orientation = Orientation.Horizontal};
                petrolPump.PresetPlaceholder.Children.Add(presetLcdStackPanel);
                this.listeners.Add(presetLcd.Listen(t =>
                    petrolPump.Dispatcher.InvokeAsync(() => petrolPump.SetLcdDigits(presetLcdStackPanel, t, 5, true))));

                StackPanel saleCostStackPanel = new StackPanel {Orientation = Orientation.Horizontal};
                petrolPump.DollarsPlaceholder.Children.Add(saleCostStackPanel);
                this.listeners.Add(saleCostLcd.Listen(t =>
                    petrolPump.Dispatcher.InvokeAsync(() => petrolPump.SetLcdDigits(saleCostStackPanel, t, 5, true))));

                StackPanel saleQuantityLcdStackPanel = new StackPanel {Orientation = Orientation.Horizontal};
                petrolPump.LitersPlaceholder.Children.Add(saleQuantityLcdStackPanel);
                this.listeners.Add(saleQuantityLcd.Listen(t =>
                    petrolPump.Dispatcher.InvokeAsync(() =>
                        petrolPump.SetLcdDigits(saleQuantityLcdStackPanel, t, 5, true))));

                StackPanel priceLcd1StackPanel = new StackPanel {Orientation = Orientation.Horizontal};
                petrolPump.Fuel1Placeholder.Children.Add(priceLcd1StackPanel);
                this.listeners.Add(priceLcd1.Listen(t =>
                    petrolPump.Dispatcher.InvokeAsync(() =>
                        petrolPump.SetLcdDigits(priceLcd1StackPanel, t, 5, false))));

                StackPanel priceLcd2StackPanel = new StackPanel {Orientation = Orientation.Horizontal};
                petrolPump.Fuel2Placeholder.Children.Add(priceLcd2StackPanel);
                this.listeners.Add(priceLcd2.Listen(t =>
                    petrolPump.Dispatcher.InvokeAsync(() =>
                        petrolPump.SetLcdDigits(priceLcd2StackPanel, t, 5, false))));

                StackPanel priceLcd3StackPanel = new StackPanel {Orientation = Orientation.Horizontal};
                petrolPump.Fuel3Placeholder.Children.Add(priceLcd3StackPanel);
                this.listeners.Add(priceLcd3.Listen(t =>
                    petrolPump.Dispatcher.InvokeAsync(() =>
                        petrolPump.SetLcdDigits(priceLcd3StackPanel, t, 5, false))));

                Dictionary<CellLoop<UpDown>, Image> nozzles = new Dictionary<CellLoop<UpDown>, Image>
                {
                    {nozzle1, petrolPump.Nozzle1Image},
                    {nozzle2, petrolPump.Nozzle2Image},
                    {nozzle3, petrolPump.Nozzle3Image}
                };
                this.listeners.AddRange(nozzles.Select(nozzle => nozzle.Key.Listen(p =>
                    petrolPump.Dispatcher.InvokeAsync(() =>
                        nozzle.Value.Margin =
                            p == UpDown.Up ? new Thickness(0, 0, 0, 0) : new Thickness(0, 30, 0, 0)))));

                foreach (KeyValuePair<CellLoop<UpDown>, Image> nozzle in nozzles)
                {
                    StreamSink<Unit> nozzleClicks = Stream.CreateSink<Unit>();
                    nozzle.Value.MouseDown += async (sender, args) =>
                    {
                        if (args.LeftButton == MouseButtonState.Pressed)
                        {
                            await Task.Run(() => nozzleClicks.Send(Unit.Value));
                        }
                    };
                    nozzle.Key.Loop(nozzleClicks
                        .Snapshot(nozzle.Key, (_, n) => n == UpDown.Down ? UpDown.Up : UpDown.Down).Hold(UpDown.Down));
                }

                this.listeners.Add(sSaleComplete.Listen(sale =>
                {
                    petrolPump.Dispatcher.InvokeAsync(() =>
                    {
                        SaleCompleteDialog dialog = new SaleCompleteDialog(
                            sale.Fuel.ToString(),
                            Formatters.FormatPrice(sale.Price, null),
                            Formatters.FormatSaleCost(sale.Cost),
                            Formatters.FormatSaleQuantity(sale.Quantity));
                        dialog.Owner = petrolPump;
                        csClearSale.Send(dialog.SOkClicked);
                        dialog.Show();
                        IListener l = null;
                        // ReSharper disable once RedundantAssignment
                        l = dialog.SOkClicked.Listen(_ =>
                        {
                            petrolPump.Dispatcher.InvokeAsync(() => dialog.Close());

                            // ReSharper disable once AccessToModifiedClosure
                            l?.Unlisten();
                        });
                    });
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
                    l.Unlisten();
                }
            }

            private static System.IO.Stream GetResourceStream(string path)
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
}