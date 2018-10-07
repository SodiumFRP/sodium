module SodiumFRP.FSharp.Func

open System

type Override<'T> = | NoOverride | Override of ('T -> 'T)

let createVirtual f = function | NoOverride -> f | Override o -> o f

let flip f x y = f y x

type private CombineDelegate<'a> = delegate of 'a -> unit
let combine<'a> x y =
    let d1 = CombineDelegate<'a> x
    let d2 = CombineDelegate<'a> y
    let del = Delegate.Combine (d1, d2) :?> CombineDelegate<'a>
    del.Invoke

type private CombineDelegate<'a, 'b> = delegate of 'a * 'b -> unit
let combine2<'a, 'b> x y =
    let d1 = CombineDelegate<'a, 'b> x
    let d2 = CombineDelegate<'a, 'b> y
    let del = Delegate.Combine (d1, d2) :?> CombineDelegate<'a, 'b>
    (fun a b -> del.Invoke (a, b))

type private CombineDelegate<'a, 'b, 'c> = delegate of 'a * 'b * 'c -> unit
let combine3<'a, 'b, 'c> x y =
    let d1 = CombineDelegate<'a, 'b, 'c> x
    let d2 = CombineDelegate<'a, 'b, 'c> y
    let del = Delegate.Combine (d1, d2) :?> CombineDelegate<'a, 'b, 'c>
    (fun a b c -> del.Invoke (a, b, c))

type private CombineDelegate<'a, 'b, 'c, 'd> = delegate of 'a * 'b * 'c * 'd -> unit
let combine4<'a, 'b, 'c, 'd> x y =
    let d1 = CombineDelegate<'a, 'b, 'c, 'd> x
    let d2 = CombineDelegate<'a, 'b, 'c, 'd> y
    let del : CombineDelegate<'a, 'b, 'c, 'd> = Delegate.Combine (d1, d2) :?> CombineDelegate<'a, 'b, 'c, 'd>
    (fun a b c d -> del.Invoke (a, b, c, d))

type private CombineDelegate<'a, 'b, 'c, 'd, 'e> = delegate of 'a * 'b * 'c * 'd * 'e -> unit
let combine5<'a, 'b, 'c, 'd, 'e> x y =
    let d1 = CombineDelegate<'a, 'b, 'c, 'd, 'e> x
    let d2 = CombineDelegate<'a, 'b, 'c, 'd, 'e> y
    let del : CombineDelegate<'a, 'b, 'c, 'd, 'e> = Delegate.Combine (d1, d2) :?> CombineDelegate<'a, 'b, 'c, 'd, 'e>
    (fun a b c d e -> del.Invoke (a, b, c, d, e))

type private CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f> = delegate of 'a * 'b * 'c * 'd * 'e * 'f -> unit
let combine6<'a, 'b, 'c, 'd, 'e, 'f> x y =
    let d1 = CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f> x
    let d2 = CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f> y
    let del : CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f> =
        Delegate.Combine (d1, d2) :?> CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f>
    (fun a b c d e f -> del.Invoke (a, b, c, d, e, f))

type private CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g> = delegate of 'a * 'b * 'c * 'd * 'e * 'f * 'g -> unit
let combine7<'a, 'b, 'c, 'd, 'e, 'f, 'g> x y =
    let d1 = CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g> x
    let d2 = CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g> y
    let del : CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g> =
        Delegate.Combine (d1, d2) :?> CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g>
    (fun a b c d e f g -> del.Invoke (a, b, c, d, e, f, g))

type private CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = delegate of 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> unit
let combine8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> x y =
    let d1 = CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> x
    let d2 = CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> y
    let del : CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> =
        Delegate.Combine (d1, d2) :?> CombineDelegate<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h>
    (fun a b c d e f g h -> del.Invoke (a, b, c, d, e, f, g, h))
