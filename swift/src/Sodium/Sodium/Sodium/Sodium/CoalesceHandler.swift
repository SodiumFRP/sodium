internal class CoalesceHandler<T>
{
    typealias Action = (Transaction, T, String) -> Void
    var accum: T?
    
    func create(f: (T,T) -> T, out: Stream<T>) -> Action {
        return { (trans1, a, dbg) in
    
            if let acc = self.accum {
                self.accum = f(acc, a)
            }
            else {
                trans1.prioritized(out.node, action: { trans2 in
                    out.send(trans2, a: self.accum!, dbg: "\(self.accum!)")
                    self.accum = nil
                }, dbg: stack(dbg))
                self.accum = a
            }
        }
    }
}
