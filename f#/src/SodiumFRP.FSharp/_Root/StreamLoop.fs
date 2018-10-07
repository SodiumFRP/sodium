namespace SodiumFRP.FSharp

type StreamLoop<'T> internal () =
    inherit Stream<'T>()
    
    let isAssignedLock = obj ()
    let mutable isAssigned = false
    
    member internal this.Loop transaction (stream : Stream<'T>) =
        lock isAssignedLock (fun () ->
            if isAssigned then invalidOp "Loop was looped more than once."
            isAssigned <- true)
        this.AttachListener <| stream.Listen this.Node this.Send |> ignore
        lock stream.KeepListenersAlive (fun () -> stream.KeepListenersAlive.Use this.KeepListenersAlive)
