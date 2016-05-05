extension Cell<Cell<T>>
{
    /// <summary>
    ///     Unwrap a cell inside another cell to give a time-varying cell implementation.
    /// </summary>
    /// <typeparam name="T">The type of the cell.</typeparam>
    /// <param name="cca">The cell containing another cell.</param>
    /// <returns>The unwrapped cell.</returns>
    static func SwitchC() -> Cell<T>
    {
        return Transaction.Apply(
            {
                trans1 in
                
                let za = self.SampleLazy().Map{ ca in ca.Sample()}
                let out = Stream<T>(self.KeepListenersAlive)
                var currentListener: IListener?
                let h = { (trans2, ca) in
                    currentListener = ca.Value(trans2).Listen(out.Node, trans2, out.Send, false)
                }
                let l1 = cca.Value(trans1).Listen(out.Node, trans1, h, false)
                
                return out.UnsafeAddCleanup(l1).HoldLazy(za)
            })
    }
}

extension Cell<Stream<T>> {
    /// <summary>
    ///     Unwrap a stream inside a cell to give a time-varying stream implementation.
    /// </summary>
    /// <typeparam name="T">The type of the stream.</typeparam>
    /// <param name="csa">The cell containing the stream.</param>
    /// <returns>The unwrapped stream.</returns>
    public static func SwitchS<T>() -> Stream<T> {
        
        return Transaction.Apply(
        {
            trans1 in
            let out = Stream<T>(self.KeepListenersAlive)
            var currentListener = csa.SampleNoTransaction().Listen(out.Node, trans1, out.Send, false)
            let h = { (trans2, sa) in
                trans2.Last({
                    currentListener.Unlisten()
                    currentListener = sa.Listen(out.Node, trans2, out.Send, true)
                })
            }
            let l1 = self.Updates(trans1).Listen(out.Node, trans1, h, false)
            return out.UnsafeAddCleanup(l1)
        })
    }
}

/*
    /// <summary>
    ///     Lift a function into cells, so the returned cell always reflects the specified function applied to the
    ///     input cells' values.
    /// </summary>
    /// <typeparam name="T">The type of the cells.</typeparam>
    /// <typeparam name="TResult">The type of the result.</typeparam>
    /// <param name="c">The enumerable of cells.</param>
    /// <param name="f">The binary function to lift into the cells.</param>
    /// <returns>A cell containing values resulting from the function applied to the input cells' values.</returns>
    public static Cell<TResult> Lift<T, TResult>(this IEnumerable<Cell<T>> c, Func<IReadOnlyList<T>, TResult> f)
    {
        return Transaction.Apply(trans1 =>
        {
            IReadOnlyList<Cell<T>> cells = c.ToArray()
            T[] values = cells.Select(cell => cell.SampleNoTransaction()).ToArray()
            Stream<TResult> @out = Stream<TResult>(FanOutKeepListenersAlive(cells.Select(cell => cell.KeepListenersAlive)))
            Lazy<TResult> initialValue = Lazy<TResult>(() => f(values.ToArray()))
            IEnumerable<IListener> listeners = cells.Select((cell, i) => cell.Updates(trans1).Listen(@out.Node, trans1, (trans2, v) =>
              {
                  values[i] = v
                  @out.Send(trans2, f(values.ToArray()))
              }, false))
            return @out.UnsafeAddCleanup(ImmutableCompositeListener(listeners)).HoldLazy(initialValue)
        })
    }

    private class FanOutKeepListenersAlive : IKeepListenersAlive
    {
        private let keepListenersAliveList: IReadOnlyList<IKeepListenersAlive>

        public init(keepListenersAliveEnumerable: Sequence)
        {
            self.keepListenersAliveList = keepListenersAliveEnumerable.ToArray()
        }

        public func KeepListenerAlive(listener: IListener)
        {
            for keepListenersAlive in self.keepListenersAliveList {
                keepListenersAlive.KeepListenerAlive(listener)
            }
        }

        public func StopKeepingListenerAlive(listener: IListener) {
            for keepListenersAlive in self.keepListenersAliveList {
                keepListenersAlive.StopKeepingListenerAlive(listener)
            }
        }

        public func Use(childKeepListenersAlive: IKeepListenersAlive) {
            for keepListenersAlive in self.keepListenersAliveList {
                keepListenersAlive.Use(childKeepListenersAlive)
            }
        }
    }
}
*/