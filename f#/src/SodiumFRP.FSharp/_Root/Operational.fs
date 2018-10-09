module SodiumFRP.FSharp.Operational

let updates behavior =
    Transaction.Apply
        (fun transaction _ -> behavior |> Behavior.updates |> Stream.coalesce transaction (fun left right -> right))
        false

let value behavior = Transaction.Apply (fun transaction _ -> behavior |> Behavior.value transaction) false

let split stream =
    let out = Stream (Stream.keepListenersAlive stream)
    let listener =
        stream
            |> Stream.listenN
                (Node<_> ())
                (fun transaction aa ->
                    let mutable childIndex = 0
                    for a in aa do
                        transaction.Split childIndex (fun transaction -> out.Send transaction a)
                        childIndex <- childIndex + 1)
    out |> Stream.ual listener

let defer stream = split (stream |> Stream.map (fun v -> [|v|]))