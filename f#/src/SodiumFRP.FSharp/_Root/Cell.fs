namespace SodiumFRP.FSharp

type Cell<'T> internal (behavior : Behavior<'T>) =

    let updates = lazy Transaction.Apply (fun transaction _ -> (behavior.Updates ()).Coalesce transaction (fun left right -> right)) false
    let getUpdates () = updates.Value
    
    member internal __.Behavior = behavior
    member internal __.Updates = getUpdates ()