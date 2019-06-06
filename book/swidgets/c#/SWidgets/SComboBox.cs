using System;
using System.Collections.Generic;
using System.Windows.Controls;
using Sodium.Frp;
using Sodium.Functional;

namespace SWidgets
{
    public class SComboBox<T> : ComboBox, IDisposable
    {
        private readonly Action disposeListeners;

        public SComboBox()
            : this(null)
        {
        }

        public SComboBox(IEnumerable<T> items)
            : this(Stream.Never<Maybe<T>>(), Maybe.None, items)
        {
        }

        public SComboBox(Maybe<T> initSelectedItem)
            : this(initSelectedItem, null)
        {
        }

        public SComboBox(Maybe<T> initSelectedItem, IEnumerable<T> items)
            : this(Stream.Never<Maybe<T>>(), initSelectedItem, items)
        {
        }

        public SComboBox(Stream<Maybe<T>> setSelectedItem, Maybe<T> initSelectedItem)
            : this(setSelectedItem, initSelectedItem, null)
        {
        }

        public SComboBox(Stream<Maybe<T>> setSelectedItem, Maybe<T> initSelectedItem, IEnumerable<T> items)
        {
            void SetSelectedItemImpl(Maybe<T> m) => base.SelectedItem = m.Match<object>(v => v, () => null);

            this.ItemsSource = items ?? new T[0];
            SetSelectedItemImpl(initSelectedItem);

            List<IListener> listeners = new List<IListener>();

            StreamSink<int> sDecrement = Stream.CreateSink<int>();
            Cell<bool> allow = setSelectedItem.Map(_ => 1).OrElse(sDecrement).Accum(0, (b, d) => b + d).Map(b => b == 0);

            Maybe<T> GetSelectedItem()
            {
                object sel = base.SelectedItem;
                return sel == null ? Maybe.None : Maybe.Some((T)sel);
            }

            StreamSink<Maybe<T>> sUserSelectedItem = Stream.CreateSink<Maybe<T>>();
            this.SUserSelectedItem = sUserSelectedItem;
            this.SelectedItem = sUserSelectedItem.Gate(allow).OrElse(setSelectedItem).Hold(initSelectedItem);

            void SelectionChangedEventHandler(object sender, SelectionChangedEventArgs args)
            {
                Maybe<T> selectedItem = GetSelectedItem();
                this.Dispatcher.InvokeAsync(() => sUserSelectedItem.Send(selectedItem));
            }

            this.SelectionChanged += SelectionChangedEventHandler;

            listeners.Add(setSelectedItem.Listen(m =>
            {
                this.Dispatcher.InvokeAsync(() =>
                {
                    this.SelectionChanged -= SelectionChangedEventHandler;
                    SetSelectedItemImpl(m);
                    this.SelectionChanged += SelectionChangedEventHandler;
                    sDecrement.Send(-1);
                });
            }));

            this.disposeListeners = () =>
            {
                foreach (IListener l in listeners)
                {
                    l.Unlisten();
                }
            };
        }

        public new Cell<Maybe<T>> SelectedItem { get; }
        public Stream<Maybe<T>> SUserSelectedItem { get; }

        public void Dispose()
        {
            this.disposeListeners();
        }
    }
}