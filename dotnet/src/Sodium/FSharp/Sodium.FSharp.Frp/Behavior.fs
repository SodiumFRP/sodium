module Sodium.Frp.Behavior

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let constant value = BehaviorInternal.ConstantImpl value

[<MethodImpl(MethodImplOptions.NoInlining)>]
let constantLazy value = BehaviorInternal.ConstantLazyImpl value

[<MethodImpl(MethodImplOptions.NoInlining)>]
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

[<MethodImpl(MethodImplOptions.NoInlining)>]
let sample (behavior : Behavior<_>) = behavior.SampleImpl ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let sampleLazy (behavior : Behavior<_>) = behavior.SampleLazyImpl ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let map f (behavior : Behavior<_>) = behavior.MapImpl (Func<_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let apply f (behavior : Behavior<_>) = behavior.ApplyImpl (f |> map (fun f -> Func<_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift2 f ((behavior : Behavior<_>), behavior2) = behavior.LiftImpl (behavior2, Func<_,_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift3 f ((behavior : Behavior<_>), behavior2, behavior3) = behavior.LiftImpl (behavior2, behavior3, Func<_,_,_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift4 f ((behavior : Behavior<_>), behavior2, behavior3, behavior4) = behavior.LiftImpl (behavior2, behavior3, behavior4, Func<_,_,_,_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift5 f ((behavior : Behavior<_>), behavior2, behavior3, behavior4, behavior5) = behavior.LiftImpl (behavior2, behavior3, behavior4, behavior5, Func<_,_,_,_,_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let lift6 f ((behavior : Behavior<_>), behavior2, behavior3, behavior4, behavior5, behavior6) = behavior.LiftImpl (behavior2, behavior3, behavior4, behavior5, behavior6, Func<_,_,_,_,_,_,_> f)

let lift7 f (behavior, behavior2, behavior3, behavior4, behavior5, behavior6, behavior7) =
    ((behavior, behavior2, behavior3, behavior4, behavior5, behavior6) |> lift6 tuple6S, behavior7) |> lift2 (fun struct (a, b, c, d, e, f') g -> f a b c d e f' g)

let lift8 f (behavior, behavior2, behavior3, behavior4, behavior5, behavior6, behavior7, behavior8) =
    ((behavior, behavior2, behavior3, behavior4, behavior5, behavior6) |> lift6 tuple6S, behavior7, behavior8) |> lift3 (fun struct (a, b, c, d, e, f') g h -> f a b c d e f' g h)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private liftAllCollection f (behaviors : IReadOnlyCollection<'Behavior>) = BehaviorExtensionMethodsInternal.LiftBehaviorsImpl (behaviors, (Func<_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private liftAllSeq f (behaviors : seq<'Behavior>) = BehaviorExtensionMethodsInternal.LiftBehaviorsImpl (behaviors, (Func<_,_> f))

let liftAll f (behaviors : seq<'Behavior>) =
    match behaviors with
    | :? IReadOnlyCollection<'Behavior> as behaviors -> liftAllCollection f behaviors
    | behaviors -> liftAllSeq f behaviors

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchB behavior = BehaviorExtensionMethodsInternal.SwitchBImpl behavior

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchC behavior = BehaviorExtensionMethodsInternal.SwitchCImpl behavior

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchS behavior = BehaviorExtensionMethodsInternal.SwitchSImpl behavior