//S: SequenceType where S.Generator.Element == T


struct OrElseSequenceSequence<T, S:CollectionType where S.Generator.Element == Stream<T>> : SequenceType {
    
    var limit : Int
    var sequence : S
    
    func generate() -> AnyGenerator<S.Generator.Element> {
        var generator = sequence.generate()
        var limit = self.limit
        
        return anyGenerator {
            guard limit > 0 else {
                return nil
            }
            
            limit = limit - 1
            
            return generator.next()
        }
    }
    
    func orElse(s: S) -> Stream<T>
    {
        return s.Merge({ (left, right) in return left })
    }

    func merge(f: (T,T) -> T) -> Stream<T> {
        let v = self.toArray()
        return Merge(v, 0, v.Count, f)
    }

    func merge(e: S, start: Int, end: Int, f: (T,T)->T) -> Stream<T> {
        let n = end - start
    
        if (n == 0)
        {
            return Stream<T>()
        }
    
        if (n == 1)
        {
            return e[start]
        }
    
        if (n == 2)
        {
            return e[start].Merge(e[start + 1], f)
        }
    
        let mid = (start + end) / 2
        return merge(e, start, mid, f).Merge(Merge(e, mid, end, f), f)
    }

}


extension SequenceType where Generator.Element == Stream {
    func take(count:Int) -> TakeFromSequenceSequence<Self> {
        return TakeFromSequenceSequence(limit: count, sequence: self)
    }
}
//public static class StreamExtensionMethods
extension SequenceType {
    /// <summary>
    ///     Merges a collection of streams and drops the stream's value specified earlier in the collection in the simultaneous
    ///     case.
    /// </summary>
    /// <param name="s">The collection of streams to merge.</param>
    /// <returns>
    ///     A stream that is the result of merging the collection of streams and dropping the stream's value specified
    ///     earlier in the collection in the simultaneous case.
    /// </returns>
    public static func orElse<T>(this IEnumerable<Stream<T>> s) -> Stream<T>
    {
        return s.Merge((left, right) => left)
    }

    /// <summary>
    ///     Merge a collection of streams of the same type into one, so that events on any input appear on the returned stream.
    /// </summary>
    /// <param name="s">The collection of streams to merge.</param>
    /// <param name="f">
    ///     Function to combine the values. It may construct FRP logic or use <see cref="Cell{T}.Sample" />.  Apart
    ///     from this the function must be pure.
    /// </param>
    /// <returns>
    ///     A stream which is the combination of event values from the collection of streams
    ///     <param name="s" />
    ///     .
    /// </returns>
    /// <remarks>
    ///     If the events are simultaneous (that is, one event from more than one stream
    ///     occurring in the same transaction), combine them into one using the specified combining function
    ///     so that the returned stream is guaranteed only ever to have one event per transaction.
    ///     The event from the stream earlier in the collection will appear at the left input of the combining function, and
    ///     the event from the stream later in the collection will appear at the right.
    /// </remarks>
    public static Stream<T> merge<T>(this IEnumerable<Stream<T>> s, Func<T, T, T> f)
    {
        IReadOnlyList<Stream<T>> v = s.ToArray()
        return Merge(v, 0, v.Count, f)
    }

    private static Stream<T> merge<T>(IReadOnlyList<Stream<T>> e, int start, int end, Func<T, T, T> f)
    {
        int n = end - start

        if (n == 0)
        {
            return new Stream<T>()
        }

        if (n == 1)
        {
            return e[start]
        }

        if (n == 2)
        {
            return e[start].Merge(e[start + 1], f)
        }

        int mid = (start + end) / 2
        return Merge(e, start, mid, f).Merge(Merge(e, mid, end, f), f)
    }

    /// <summary>
    ///     Return a stream that only outputs events that have values, removing the <see cref="IMaybe{T}" /> wrapper, and
    ///     discarding <see cref="Maybe.Nothing{T}()" /> values.
    /// </summary>
    /// <param name="s">The stream of <see cref="IMaybe{T}" /> values to filter.</param>
    /// <returns>
    ///     A stream that only outputs events that have values, removing the <see cref="IMaybe{T}" /> wrapper, and
    ///     discarding <see cref="Maybe.Nothing{T}()" /> values.
    /// </returns>
    public static Stream<T> filterMaybe<T>(this Stream<IMaybe<T>> s)
    {
        Stream<T> @out = new Stream<T>(s.KeepListenersAlive)
        IListener l = s.Listen(@out.Node, (trans2, a) => a.Match(v => @out.Send(trans2, v), () => { }))
        return @out.UnsafeAddCleanup(l)
    }
}
