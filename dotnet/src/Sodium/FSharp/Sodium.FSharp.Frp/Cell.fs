module Sodium.Frp.Cell

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let constant value = CellInternal.ConstantImpl value

[<MethodImpl(MethodImplOptions.NoInlining)>]
let constantLazy value = CellInternal.ConstantLazyImpl value

[<MethodImpl(MethodImplOptions.NoInlining)>]
let loop f =
    TransactionInternal.Apply
        (
            (fun transaction _ ->
                let l = LoopedCell ()
                let struct (s, r) = f l
                l.Loop (transaction, s)
                struct (s, r)),
            false
        )

let loopWithNoCaptures f =
    let struct (l, _) = loop (fun s -> struct (f s, ()))
    l

[<MethodImpl(MethodImplOptions.NoInlining)>]
let sample (cell : Cell<_>) = cell.SampleImpl ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let sampleLazy (cell : Cell<_>) = cell.SampleLazyImpl ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let updates (cell : Cell<_>) = cell.UpdatesImpl

[<MethodImpl(MethodImplOptions.NoInlining)>]
let values (cell : Cell<_>) = cell.ValuesImpl

[<MethodImpl(MethodImplOptions.NoInlining)>]
let asBehavior (cell : Cell<_>) = cell.BehaviorImpl

[<MethodImpl(MethodImplOptions.NoInlining)>]
let listenWeak handler (cell : Cell<_>) = cell.ListenWeakImpl (Action<_> handler)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let listen handler (cell : Cell<_>) = cell.ListenImpl (Action<_> handler)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let map f (cell : Cell<_>) = cell.MapImpl (Func<_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let apply f (cell : Cell<_>) = cell.ApplyImpl (f |> map (fun f -> Func<_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift2 f ((cell : Cell<_>), cell2) = cell.LiftImpl (cell2, Func<_,_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift3 f ((cell : Cell<_>), cell2, cell3) = cell.LiftImpl (cell2, cell3, Func<_,_,_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift4 f ((cell : Cell<_>), cell2, cell3, cell4) = cell.LiftImpl (cell2, cell3, cell4, Func<_,_,_,_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift5 f ((cell : Cell<_>), cell2, cell3, cell4, cell5) = cell.LiftImpl (cell2, cell3, cell4, cell5, Func<_,_,_,_,_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift6 f ((cell : Cell<_>), cell2, cell3, cell4, cell5, cell6) = cell.LiftImpl (cell2, cell3, cell4, cell5, cell6, Func<_,_,_,_,_,_,_> f)

let lift7 f (cell, cell2, cell3, cell4, cell5, cell6, cell7) =
    ((cell, cell2, cell3, cell4, cell5, cell6) |> lift6 tuple6S, cell7) |> lift2 (fun struct (a, b, c, d, e, f') g -> f a b c d e f' g)

let lift8 f (cell, cell2, cell3, cell4, cell5, cell6, cell7, cell8) =
    ((cell, cell2, cell3, cell4, cell5, cell6) |> lift6 tuple6S, cell7, cell8) |> lift3 (fun struct (a, b, c, d, e, f') g h -> f a b c d e f' g h)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let calmWithCompare compare (cell : Cell<_>) = cell.CalmImpl (Func<_,_,_> compare)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let calmWithEqualityComparer (equalityComparer : IEqualityComparer<_>) (cell : Cell<_>) = cell.CalmImpl (Func<_,_,_> (fun x y -> equalityComparer.Equals (x, y)))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let calm (cell : Cell<_>) = cell.CalmImpl (Func<_,_,_> (=))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private liftAllCollection f (cells : IReadOnlyCollection<'Cell>) = CellExtensionMethodsInternal.LiftCellsImpl (cells, (Func<_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private liftAllSeq f (cells : seq<'Cell>) = CellExtensionMethodsInternal.LiftCellsImpl (cells, (Func<_,_> f))

let liftAll f (cells : seq<'Cell>) =
    match cells with
    | :? IReadOnlyCollection<'Cell> as cells -> liftAllCollection f cells
    | cells -> liftAllSeq f cells

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchB cell = CellExtensionMethodsInternal.SwitchBImpl cell

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchC cell = CellExtensionMethodsInternal.SwitchCImpl cell

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchS cell = CellExtensionMethodsInternal.SwitchSImpl cell