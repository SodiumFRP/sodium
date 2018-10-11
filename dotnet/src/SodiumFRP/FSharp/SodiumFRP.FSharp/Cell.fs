module SodiumFRP.Cell

open System
open System.Collections.Generic

let constant value = CellInternal.ConstantImpl value
let constantLazy value = CellInternal.ConstantLazyImpl value

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

let sample (cell : Cell<_>) = cell.SampleImpl ()
let sampleLazy (cell : Cell<_>) = cell.SampleLazyImpl ()
let updates (cell : Cell<_>) = cell.UpdatesImpl
let values (cell : Cell<_>) = cell.ValuesImpl
let asBehavior (cell : Cell<_>) = cell.BehaviorImpl
let listenWeak handler (cell : Cell<_>) = cell.ListenWeakImpl (Action<_> handler)
let listen handler (cell : Cell<_>) = cell.ListenImpl (Action<_> handler)
let map f (cell : Cell<_>) = cell.MapImpl (Func<_,_> f)

let apply f (cell : Cell<_>) = cell.ApplyImpl (f |> map (fun f -> Func<_,_> f))

let lift2 f ((cell : Cell<_>), cell2) = cell.LiftImpl (cell2, Func<_,_,_> f)
let lift3 f ((cell : Cell<_>), cell2, cell3) = cell.LiftImpl (cell2, cell3, Func<_,_,_,_> f)
let lift4 f ((cell : Cell<_>), cell2, cell3, cell4) = cell.LiftImpl (cell2, cell3, cell4, Func<_,_,_,_,_> f)
let lift5 f ((cell : Cell<_>), cell2, cell3, cell4, cell5) = cell.LiftImpl (cell2, cell3, cell4, cell5, Func<_,_,_,_,_,_> f)
let lift6 f ((cell : Cell<_>), cell2, cell3, cell4, cell5, cell6) = cell.LiftImpl (cell2, cell3, cell4, cell5, cell6, Func<_,_,_,_,_,_,_> f)

let lift7 f (cell, cell2, cell3, cell4, cell5, cell6, cell7) =
    ((cell, cell2, cell3, cell4, cell5, cell6) |> lift6 tuple6S, cell7) |> lift2 (fun struct (a, b, c, d, e, f') g -> f a b c d e f' g)

let lift8 f (cell, cell2, cell3, cell4, cell5, cell6, cell7, cell8) =
    ((cell, cell2, cell3, cell4, cell5, cell6) |> lift6 tuple6S, cell7, cell8) |> lift3 (fun struct (a, b, c, d, e, f') g h -> f a b c d e f' g h)

let calmWithCompare compare (cell : Cell<_>) = cell.CalmImpl (Func<_,_,_> compare)

let calmWithEqualityComparer (equalityComparer : IEqualityComparer<_>) cell =
    cell |> calmWithCompare (fun x y -> equalityComparer.Equals (x, y))

let calm cell = cell |> calmWithCompare (=)

let liftAll f (cells : seq<'Cell>) =
    match cells with
    | :? IReadOnlyCollection<'Cell> as cells -> CellExtensionMethodsInternal.LiftCellsImpl (cells, (Func<_,_> f))
    | cells -> CellExtensionMethodsInternal.LiftCellsImpl (cells, (Func<_,_> f))

let switchB cell = CellExtensionMethodsInternal.SwitchBImpl cell
let switchC cell = CellExtensionMethodsInternal.SwitchCImpl cell
let switchS cell = CellExtensionMethodsInternal.SwitchSImpl cell