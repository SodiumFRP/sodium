internal class CoalesceHandler<T>
{
    var accum: T?
    func create(f: (T,T) -> T, out: Stream<T>) -> (Transaction, T) -> Void {
        
        return { (trans1, a) in
    
            if let acc = self.accum {
                self.accum = f(acc, a)
            }
            else {
                trans1.prioritized(out.node, action: { trans2 in
                    out.send(trans2, a: self.accum!)
                    self.accum = nil
                }, dbg: "CoalesceHandler")
                self.accum = a
            }
        }
    }
}
