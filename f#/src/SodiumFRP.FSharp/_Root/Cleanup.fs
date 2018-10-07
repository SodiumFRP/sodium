namespace SodiumFRP.FSharp

type Cleanup (cleanup) =
    let mutable stream = Some (Stream<unit>().AttachListener <| Listener.fromAction cleanup)
    member internal __.CleanupNow () = stream <- None
    member internal __.Stream = stream // prevent GC

module Cleanup =
    let cleanupNow (cleanup : Cleanup) = cleanup.CleanupNow ()