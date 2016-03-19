module Sodium.Lazy

let map f (l : 'T Lazy) = lazy (f l.Value)

let lift2 f (l1 : 'T1 Lazy) (l2 : 'T2 Lazy) =
    lazy (f l1.Value l2.Value)

let lift3 f (l1 : 'T1 Lazy) (l2 : 'T2 Lazy) (l3 : 'T3 Lazy) =
    lazy (f l1.Value l2.Value l3.Value)

let lift4 f (l1 : 'T1 Lazy) (l2 : 'T2 Lazy) (l3 : 'T3 Lazy) (l4 : 'T4 Lazy) =
    lazy (f l1.Value l2.Value l3.Value l4.Value)

let lift5 f (l1 : 'T1 Lazy) (l2 : 'T2 Lazy) (l3 : 'T3 Lazy) (l4 : 'T4 Lazy) (l5 : 'T5 Lazy) =
    lazy (f l1.Value l2.Value l3.Value l4.Value l5.Value)

let lift6 f (l1 : 'T1 Lazy) (l2 : 'T2 Lazy) (l3 : 'T3 Lazy) (l4 : 'T4 Lazy) (l5 : 'T5 Lazy) (l6 : 'T6 Lazy) =
    lazy (f l1.Value l2.Value l3.Value l4.Value l5.Value l6.Value)

let lift7 f (l1 : 'T1 Lazy) (l2 : 'T2 Lazy) (l3 : 'T3 Lazy) (l4 : 'T4 Lazy) (l5 : 'T5 Lazy) (l6 : 'T6 Lazy) (l7 : 'T7 Lazy) =
    lazy (f l1.Value l2.Value l3.Value l4.Value l5.Value l6.Value l7.Value)
    
let lift8 f (l1 : 'T1 Lazy) (l2 : 'T2 Lazy) (l3 : 'T3 Lazy) (l4 : 'T4 Lazy) (l5 : 'T5 Lazy) (l6 : 'T6 Lazy) (l7 : 'T7 Lazy) (l8 : 'T8 Lazy) =
    lazy (f l1.Value l2.Value l3.Value l4.Value l5.Value l6.Value l7.Value l8.Value)