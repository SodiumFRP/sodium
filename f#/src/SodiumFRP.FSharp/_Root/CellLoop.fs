namespace SodiumFRP.FSharp

type CellLoop<'T> =
    inherit Cell<'T>
    
    val private behaviorLoop : BehaviorLoop<'T>
    
    internal new () =
        let behaviorLoop = BehaviorLoop<_> ()
        { inherit Cell<_> (behaviorLoop); behaviorLoop = behaviorLoop }
    
    member internal this.Loop transaction (cell : Cell<'T>) = this.behaviorLoop.Loop transaction cell.Behavior