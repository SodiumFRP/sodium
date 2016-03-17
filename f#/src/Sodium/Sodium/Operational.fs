namespace Sodium

module Operational =
    
    let updates (cell : 'T Cell) = Transaction.Apply cell.Updates

    let value (cell : 'T Cell) = Transaction.Apply (fun transaction -> cell |> Cell.valueInternal transaction)

    let split (stream : #('T seq) Stream) =
        let out = new Stream<'T>()
        let listener = stream.ListenWithTransaction out.Node (fun transaction aa ->
            let mutable childIndex = 0
            for a in aa do
                transaction.Post(childIndex, (fun transaction -> out.Send(transaction, a)))
                childIndex <- childIndex + 1)
        out.UnsafeAddCleanup listener

    let defer (stream : 'T Stream) = split (stream |> Stream.map (fun v -> [v]))