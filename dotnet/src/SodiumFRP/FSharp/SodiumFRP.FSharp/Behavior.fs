module SodiumFRP.Behavior

open System
open System.Collections.Generic

let constant value = BehaviorInternal.ConstantImpl value
let constantLazy value = BehaviorInternal.ConstantLazyImpl value

let loop f =
    TransactionInternal.Apply
        (
            (fun transaction _ ->
                let l = LoopedBehavior ()
                let struct (s, r) = f l
                l.Loop (transaction, s)
                struct (s, r)),
            false
        )

let loopWithNoCaptures f =
    let struct (l, _) = loop (fun s -> struct (f s, ()))
    l

let sample (behavior : Behavior<_>) = behavior.SampleImpl ()
let sampleLazy (behavior : Behavior<_>) = behavior.SampleLazyImpl ()
let map f (behavior : Behavior<_>) = behavior.MapImpl (Func<_,_> f)

let apply f (behavior : Behavior<_>) = behavior.ApplyImpl (f |> map (fun f -> Func<_,_> f))

let lift2 f ((behavior : Behavior<_>), behavior2) = behavior.LiftImpl (behavior2, Func<_,_,_> f)
let lift3 f ((behavior : Behavior<_>), behavior2, behavior3) = behavior.LiftImpl (behavior2, behavior3, Func<_,_,_,_> f)
let lift4 f ((behavior : Behavior<_>), behavior2, behavior3, behavior4) = behavior.LiftImpl (behavior2, behavior3, behavior4, Func<_,_,_,_,_> f)
let lift5 f ((behavior : Behavior<_>), behavior2, behavior3, behavior4, behavior5) = behavior.LiftImpl (behavior2, behavior3, behavior4, behavior5, Func<_,_,_,_,_,_> f)
let lift6 f ((behavior : Behavior<_>), behavior2, behavior3, behavior4, behavior5, behavior6) = behavior.LiftImpl (behavior2, behavior3, behavior4, behavior5, behavior6, Func<_,_,_,_,_,_,_> f)

let lift7 f (behavior, behavior2, behavior3, behavior4, behavior5, behavior6, behavior7) =
    ((behavior, behavior2, behavior3, behavior4, behavior5, behavior6) |> lift6 tuple6S, behavior7) |> lift2 (fun struct (a, b, c, d, e, f') g -> f a b c d e f' g)

let lift8 f (behavior, behavior2, behavior3, behavior4, behavior5, behavior6, behavior7, behavior8) =
    ((behavior, behavior2, behavior3, behavior4, behavior5, behavior6) |> lift6 tuple6S, behavior7, behavior8) |> lift3 (fun struct (a, b, c, d, e, f') g h -> f a b c d e f' g h)

let liftAll f (behaviors : seq<'Behavior>) =
    match behaviors with
    | :? IReadOnlyCollection<'Behavior> as behaviors -> BehaviorExtensionMethodsInternal.LiftBehaviorsImpl (behaviors, (Func<_,_> f))
    | behaviors -> BehaviorExtensionMethodsInternal.LiftBehaviorsImpl (behaviors, (Func<_,_> f))

let switchB behavior = BehaviorExtensionMethodsInternal.SwitchBImpl behavior
let switchC behavior = BehaviorExtensionMethodsInternal.SwitchCImpl behavior
let switchS behavior = BehaviorExtensionMethodsInternal.SwitchSImpl behavior