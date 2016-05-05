internal class CoalesceHandler
{
    internal static func create<T>(f: (T,T) -> T, out: Stream<T>) -> (Transaction, T) -> Void {
        var accum: T?
        
        return { (trans1, a) in
    
            if let acc = accum {
                accum = f(acc, a)
            }
            else {
                accum = a

                trans1.prioritized(out.node) { trans2 in
                    out.send(trans2, a: accum!)
                    accum = nil
                }
            }
        }
    }
}
