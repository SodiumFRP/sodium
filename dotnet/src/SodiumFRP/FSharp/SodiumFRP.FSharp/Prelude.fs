namespace SodiumFRP

[<AutoOpen>]
module Prelude =

    let inline fstS struct (x, _) = x
    let inline sndS struct (_, y) = y

    let inline tuple2 a b = a, b
    let inline tuple3 a b c = a, b, c
    let inline tuple4 a b c d = a, b, c, d
    let inline tuple5 a b c d e = a, b, c, d, e
    let inline tuple6 a b c d e f = a, b, c, d, e, f
    let inline tuple7 a b c d e f g = a, b, c, d, e, f, g
    let inline tuple8 a b c d e f g h = a, b, c, d, e, f, g, h

    let inline tuple2S a b = struct (a, b)
    let inline tuple3S a b c = struct (a, b, c)
    let inline tuple4S a b c d = struct (a, b, c, d)
    let inline tuple5S a b c d e = struct (a, b, c, d, e)
    let inline tuple6S a b c d e f = struct (a, b, c, d, e, f)
    let inline tuple7S a b c d e f g = struct (a, b, c, d, e, f, g)
    let inline tuple8S a b c d e f g h = struct (a, b, c, d, e, f, g, h)