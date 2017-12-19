using System;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using Sodium;
using SWidgets;

namespace Lookup
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            Transaction.RunVoid(() =>
            {
                STextBox word = new STextBox(string.Empty);
                DiscreteCellLoop<bool> enabled = new DiscreteCellLoop<bool>();
                SButton button = new SButton(enabled) { Content = "look up" };
                Stream<string> sWord = button.SClicked.Snapshot(word.Text);
                IsBusy<string, Maybe<string>> ib = new IsBusy<string, Maybe<string>>(Lookup, sWord);
                Stream<string> sDefinition = ib.SOut.Map(o => o.Match(v => v, () => "ERROR!"));
                DiscreteCell<string> definition = sDefinition.Hold(string.Empty);
                DiscreteCell<string> output = definition.Lift(ib.Busy, (def, bsy) => bsy ? "Looking up..." : def);
                enabled.Loop(ib.Busy.Map(b => !b));
                STextBox outputArea = new STextBox(output.Values, string.Empty, enabled) { TextWrapping = TextWrapping.Wrap, AcceptsReturn = true };
                this.TextBoxPlaceholder.Child = word;
                this.ButtonPlaceholder.Child = button;
                this.OutputPlaceholder.Child = outputArea;
            });
        }

        public static Stream<Maybe<string>> Lookup(Stream<string> sWord)
        {
            StreamSink<Maybe<string>> sDefinition = new StreamSink<Maybe<string>>();
            IListener listener = sWord.Listen(wrd =>
            {
                Task.Run(() =>
                {
                    //System.out.println("look up " + wrd);
                    Maybe<string> def = Maybe.None;
                    try
                    {
                        Socket s = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
                        s.Connect("dict.org", 2628);
                        NetworkStream ns = new NetworkStream(s);
                        StreamReader r = new StreamReader(ns);
                        StreamWriter w = new StreamWriter(ns);
                        try
                        {
                            r.ReadLine();
                            w.WriteLine("DEFINE ! " + wrd);
                            w.Flush();

                            string result = r.ReadLine();

                            if (result != null && result.StartsWith("150"))
                            {
                                result = r.ReadLine();
                            }

                            if (result != null && result.StartsWith("151"))
                            {
                                StringBuilder b = new StringBuilder();
                                while (true)
                                {
                                    string l = r.ReadLine();
                                    if (l == ".")
                                    {
                                        break;
                                    }
                                    b.AppendLine(l);
                                }
                                def = Maybe.Some(b.ToString());
                            }
                            else
                            {
                                MessageBox.Show("ERROR: " + result);
                            }
                        }
                        finally
                        {
                            try
                            {
                                s.Close();
                                s.Dispose();
                            }
                            catch
                            {
                                // ignored
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        MessageBox.Show("ERROR: " + e);
                    }
                    finally
                    {
                        sDefinition.Send(def);
                    }
                });
            });
            return sDefinition.AttachListener(listener);
        }

        private class IsBusy<T, TResult>
        {
            public IsBusy(Func<Stream<T>, Stream<TResult>> action, Stream<T> sIn)
            {
                this.SOut = action(sIn);
                this.Busy = sIn.Map(_ => true).OrElse(this.SOut.Map(_ => false)).Hold(false);
            }

            public Stream<TResult> SOut { get; }
            public DiscreteCell<bool> Busy { get; }
        }
    }
}