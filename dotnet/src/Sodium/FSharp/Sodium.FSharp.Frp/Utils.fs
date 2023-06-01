namespace Sodium.Frp

[<AutoOpen>]
module internal Utils =

    let inline internal tuple2 a b = (a, b)
    let inline internal tuple3 a b c = (a, b, c)
    let inline internal tuple4 a b c d = (a, b, c, d)
    let inline internal tuple5 a b c d e = (a, b, c, d, e)
    let inline internal tuple6 a b c d e f = (a, b, c, d, e, f)
    let inline internal tuple7 a b c d e f g = (a, b, c, d, e, f, g)
    let inline internal tuple8 a b c d e f g h = (a, b, c, d, e, f, g, h)
    
    let inline internal tuple2S a b = struct (a, b)
    let inline internal tuple3S a b c = struct (a, b, c)
    let inline internal tuple4S a b c d = struct (a, b, c, d)
    let inline internal tuple5S a b c d e = struct (a, b, c, d, e)
    let inline internal tuple6S a b c d e f = struct (a, b, c, d, e, f)
    let inline internal tuple7S a b c d e f g = struct (a, b, c, d, e, f, g)
    let inline internal tuple8S a b c d e f g h = struct (a, b, c, d, e, f, g, h)