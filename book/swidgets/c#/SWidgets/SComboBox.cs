using System;
using System.Collections.Generic;
using System.Windows.Controls;
using Sodium;

namespace SWidgets
{
    public class SComboBox<T> : ComboBox, IDisposable
    {
        private readonly Action disposeListeners;

        public SComboBox()
            : this((IEnumerable<T>)null)
        {
        }

        public SComboBox(IEnumerable<T> items)
            : this(Stream.Never<IMaybe<T>>(), Maybe.Nothing<T>(), items)
        {
        }

        public SComboBox(IMaybe<T> initSelectedItem)
            : this(initSelectedItem, null)
        {
        }

        public SComboBox(IMaybe<T> initSelectedItem, IEnumerable<T> items)
            : this(Stream.Never<IMaybe<T>>(), initSelectedItem, items)
        {
        }

        public SComboBox(Stream<IMaybe<T>> setSelectedItem, IMaybe<T> initSelectedItem)
            : this(setSelectedItem, initSelectedItem, null)
        {
        }

        public SComboBox(Stream<IMaybe<T>> setSelectedItem, IMaybe<T> initSelectedItem, IEnumerable<T> items)
        {
            Action<IMaybe<T>> setSelectedItemImpl = m => base.SelectedItem = m.Match<object>(v => v, () => null);

            this.ItemsSource = items ?? new T[0];
            setSelectedItemImpl(initSelectedItem);

            List<IListener> listeners = new List<IListener>();

            StreamSink<int> sDecrement = new StreamSink<int>();
            Cell<bool> allow = setSelectedItem.Map(_ => 1).OrElse(sDecrement).Accum(0, (b, d) => b + d).Map(b => b == 0);

            Func<IMaybe<T>> getSelectedItem = () =>
            {
                object sel = base.SelectedItem;
                return sel == null ? Maybe.Nothing<T>() : Maybe.Just((T)sel);
            };

            StreamSink<IMaybe<T>> sUserSelectedItem = new StreamSink<IMaybe<T>>();
            this.SUserSelectedItem = sUserSelectedItem;
            this.SelectedItem = sUserSelectedItem.Gate(allow).OrElse(setSelectedItem).Hold(initSelectedItem);

            SelectionChangedEventHandler selectionChangedEventHandler = (sender, args) =>
            {
                IMaybe<T> selectedItem = getSelectedItem();
                this.Dispatcher.InvokeAsync(() => sUserSelectedItem.Send(selectedItem));
            };

            this.SelectionChanged += selectionChangedEventHandler;

            listeners.Add(setSelectedItem.Listen(m =>
            {
                this.Dispatcher.InvokeAsync(() =>
                {
                    this.SelectionChanged -= selectionChangedEventHandler;
                    setSelectedItemImpl(m);
                    this.SelectionChanged += selectionChangedEventHandler;
                    sDecrement.Send(-1);
                });
            }));

            this.disposeListeners = () =>
            {
                foreach (IListener l in listeners)
                {
                    using (l)
                    {
                    }
                }
            };
        }

        public new Cell<IMaybe<T>> SelectedItem { get; }
        public Stream<IMaybe<T>> SUserSelectedItem { get; }

        public void Dispose()
        {
            this.disposeListeners();
        }
    }
}