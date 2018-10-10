module SodiumFRP.FSharp.Cell

open System.Collections.Generic
open SodiumFRP.FSharp

let constant value = Cell(Behavior.constant value)
let constantLazy value : Cell<_> = Cell(Behavior.constantLazy value)

let loop f =
    Transaction.Apply
        (fun transaction _ ->
            let l = CellLoop ()
            let struct (s, r) = f l
            l.Loop transaction s
            struct (s, r))
        false

let loopWithNoCaptures f =
    let struct (l, _) = loop (fun s -> struct (f s, ()))
    l

let private beh (cell : Cell<_>) = cell.Behavior

let sample cell = cell |> beh |> Behavior.sample
let sampleLazy cell = cell |> beh |> Behavior.sampleLazy

let updates (cell : Cell<_>) = cell.Updates
let values cell = Transaction.Apply (fun transaction _ -> cell |> beh |> Behavior.value transaction) false
let asBehavior cell = cell |> beh

let listenWeak handler cell =
    Transaction.Apply
        (fun transaction _ -> cell |> beh |> Behavior.value transaction |> Stream.listenWeak handler) false

let listen handler cell =
    Transaction.Apply (fun transaction _ -> cell |> beh |> Behavior.value transaction |> Stream.listen handler) false

let apply f cell = Cell(cell |> beh |> Behavior.apply (f |> beh))
let map f cell = Cell(cell |> beh |> Behavior.map f)
let lift2 f (cell, cell2) = Cell((cell |> beh, cell2 |> beh) |> Behavior.lift2 f)
let lift3 f (cell, cell2, cell3) = Cell((cell |> beh, cell2 |> beh, cell3 |> beh) |> Behavior.lift3 f)

let lift4 f (cell, cell2, cell3, cell4) =
    Cell((cell |> beh, cell2 |> beh, cell3 |> beh, cell4 |> beh) |> Behavior.lift4 f)

let lift5 f (cell, cell2, cell3, cell4, cell5) =
    Cell((cell |> beh, cell2 |> beh, cell3 |> beh, cell4 |> beh, cell5 |> beh) |> Behavior.lift5 f)

let lift6 f (cell, cell2, cell3, cell4, cell5, cell6) =
    Cell((cell |> beh, cell2 |> beh, cell3 |> beh, cell4 |> beh, cell5 |> beh, cell6 |> beh) |> Behavior.lift6 f)

let lift7 f (cell, cell2, cell3, cell4, cell5, cell6, cell7) =
    Cell((cell |> beh, cell2 |> beh, cell3 |> beh, cell4 |> beh, cell5 |> beh, cell6 |> beh, cell7 |> beh) |> Behavior.lift7 f)

let lift8 f (cell, cell2, cell3, cell4, cell5, cell6, cell7, cell8) =
    Cell((cell |> beh, cell2 |> beh, cell3 |> beh, cell4 |> beh, cell5 |> beh, cell6 |> beh, cell7 |> beh, cell8 |> beh) |> Behavior.lift8 f)

let calmWithCompare compare cell =
    let initialValue = cell |> sampleLazy
    cell |> updates |> Stream.calmInternal (initialValue |> Lazy.map Some) compare |> Stream.holdLazy initialValue

let calmWithEqualityComparer (equalityComparer : IEqualityComparer<_>) cell =
    cell |> calmWithCompare (fun x y -> equalityComparer.Equals (x, y))

let calm cell = cell |> calmWithCompare (=)

let liftAll f cells = Cell(cells |> Seq.map beh |> Array.ofSeq |> Behavior.liftAllInternal f)

let switchB cell = cell |> beh |> Behavior.switchB
let switchC cell = cell |> beh |> Behavior.switchC
let switchS cell = cell |> beh |> Behavior.switchS