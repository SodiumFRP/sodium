[<AutoOpen>]
module Sodium.Frp.Builder

open Sodium.Frp

type TransformBuilder() =
    member _.Yield m = m
    
    member _.For(m,f) = Stream.map f m
    [<CustomOperation("where",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Where(state, [<ProjectionParameter>] f) = Stream.filter f state
    [<CustomOperation("whereSome",AllowIntoPattern=true)>]
    member _.WhereSome(state) = Stream.filterOption state
    [<CustomOperation("select",AllowIntoPattern=true)>]
    member _.Select(state, [<ProjectionParameter>] f) = Stream.map f state
    [<CustomOperation("hold",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Hold(state, value) = Stream.hold value state
    [<CustomOperation("holdLazy",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.HoldLazy(state, value) = Stream.holdLazy value state
    [<CustomOperation("orElse",AllowIntoPattern=true)>]
    member _.OrElse(state, value) = state ||| value
    [<CustomOperation("orElseAll",AllowIntoPattern=true)>]
    member _.OrElseAll(state) = Stream.orElseAll state
    [<CustomOperation("mergeAll",AllowIntoPattern=true)>]
    member _.MergeAll(state, f) = Stream.mergeAll f state
    [<CustomOperation("gate",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Gate(state, value) = Stream.gate value state
    [<CustomOperation("gate",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Gate(state, value) = Stream.gateB value state
    [<CustomOperation("once",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Once(state) = Stream.once state
    [<CustomOperation("calm",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Calm state = Stream.calm state
    [<CustomOperation("calm",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Calm(state,compare) = Stream.calmWithCompare compare state
    [<CustomOperation("calm",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Calm(state,equalityComparer) = Stream.calmWithEqualityComparer equalityComparer state
    
    member _.For(m,f) = Cell.map f m
    [<CustomOperation("select",AllowIntoPattern=true)>]
    member _.Select(state, [<ProjectionParameter>] f) = Cell.map f state
    [<CustomOperation("switch",AllowIntoPattern=true)>]
    member _.Switch(state : Cell<Behavior<_>>) = Cell.switchB state
    [<CustomOperation("switch",AllowIntoPattern=true)>]
    member _.Switch(state : Cell<Cell<_>>) = Cell.switchC state
    [<CustomOperation("switch",AllowIntoPattern=true)>]
    member _.Switch(state : Cell<Stream<_>>) = Cell.switchS state
    [<CustomOperation("switchC",AllowIntoPattern=true)>]
    member _.SwitchC(state : Cell<#Cell<_>>) = Cell.switchC state
    [<CustomOperation("switchB",AllowIntoPattern=true)>]
    member _.SwitchB(state : Cell<#Behavior<_>>) = Cell.switchB state
    [<CustomOperation("switchS",AllowIntoPattern=true)>]
    member _.SwitchS(state : Cell<#Stream<_>>) = Cell.switchS state
    [<CustomOperation("calm",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Calm state = Cell.calm state
    [<CustomOperation("calm",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Calm(state,compare) = Cell.calmWithCompare compare state
    [<CustomOperation("calm",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Calm(state,equalityComparer) = Cell.calmWithEqualityComparer equalityComparer state
    [<CustomOperation("values",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Values(state) = Cell.values state
    [<CustomOperation("updates",MaintainsVariableSpace=true,AllowIntoPattern=true)>]
    member _.Updates(state) = Cell.updates state
    
    member _.For(m,f) = Behavior.map f m
    [<CustomOperation("select",AllowIntoPattern=true)>]
    member _.Select(state, [<ProjectionParameter>] f) = Behavior.map f state
    [<CustomOperation("switch",AllowIntoPattern=true)>]
    member _.Switch(state : Behavior<Cell<_>>) = Behavior.switchC state
    [<CustomOperation("switch",AllowIntoPattern=true)>]
    member _.Switch(state : Behavior<Behavior<_>>) = Behavior.switchB state
    [<CustomOperation("switch",AllowIntoPattern=true)>]
    member _.Switch(state : Behavior<Stream<_>>) = Behavior.switchS state
    [<CustomOperation("switchC",AllowIntoPattern=true)>]
    member _.SwitchC(state : Behavior<#Cell<_>>) = Behavior.switchC state
    [<CustomOperation("switchB",AllowIntoPattern=true)>]
    member _.SwitchB(state : Behavior<#Behavior<_>>) = Behavior.switchB state
    [<CustomOperation("switchS",AllowIntoPattern=true)>]
    member _.SwitchS(state : Behavior<#Stream<_>>) = Behavior.switchS state

let transform = TransformBuilder()

let inline private tuple2C c1 c2 = Cell.lift2 tuple2 (c1,c2)
let inline private tuple3C c1 c2 c3 = Cell.lift3 tuple3 (c1,c2,c3)
let inline private tuple4C c1 c2 c3 c4 = Cell.lift4 tuple4 (c1,c2,c3,c4)
let inline private tuple5C c1 c2 c3 c4 c5 = Cell.lift5 tuple5 (c1,c2,c3,c4,c5)
let inline private tuple6C c1 c2 c3 c4 c5 c6 = Cell.lift6 tuple6 (c1,c2,c3,c4,c5,c6)

let inline private tuple2B b1 b2 = Behavior.lift2 tuple2 (b1,b2)
let inline private tuple3B b1 b2 b3 = Behavior.lift3 tuple3 (b1,b2,b3)
let inline private tuple4B b1 b2 b3 b4 = Behavior.lift4 tuple4 (b1,b2,b3,b4)
let inline private tuple5B b1 b2 b3 b4 b5 = Behavior.lift5 tuple5 (b1,b2,b3,b4,b5)
let inline private tuple6B b1 b2 b3 b4 b5 b6 = Behavior.lift6 tuple6 (b1,b2,b3,b4,b5,b6)

type MergeBuilder() =
    member _.Bind2Return(m1,m2,f) = Stream.merge (fun m1 m2 -> f(m1,m2)) (m1,m2)

let merge = MergeBuilder()

type MergeAllBuilder() =
    member _.BindReturn(m,f) = Stream.mergeAll (fun l r -> f(l,r)) m

let mergeAll = MergeAllBuilder()

type LiftAllBuilder() =
    member _.BindReturn(m,f) = Cell.liftAll f m

let liftAll = LiftAllBuilder()

type LiftBuilder() =
    member _.BindReturn(m,f) = Cell.map f m
    member _.Bind2Return(m1,m2,f) = Cell.lift2 (fun m1 m2 -> f(m1,m2)) (m1,m2)
    member _.Bind3Return(m1,m2,m3,f) = Cell.lift3 (fun m1 m2 m3 -> f(m1,m2,m3)) (m1,m2,m3)
    member _.Bind4Return(m1,m2,m3,m4,f) = Cell.lift4 (fun m1 m2 m3 m4 -> f(m1,m2,m3,m4)) (m1,m2,m3,m4)
    member _.Bind5Return(m1,m2,m3,m4,m5,f) = Cell.lift5 (fun m1 m2 m3 m4 m5 -> f(m1,m2,m3,m4,m5)) (m1,m2,m3,m4,m5)
    member _.Bind6Return(m1,m2,m3,m4,m5,m6,f) = Cell.lift6 (fun m1 m2 m3 m4 m5 m6 -> f(m1,m2,m3,m4,m5,m6)) (m1,m2,m3,m4,m5,m6)
    member _.Bind7Return(m1,m2,m3,m4,m5,m6,m7,f) = Cell.lift6 (fun (m1,m2) m3 m4 m5 m6 m7 -> f(m1,m2,m3,m4,m5,m6,m7)) (tuple2C m1 m2,m3,m4,m5,m6,m7)
    member _.Bind8Return(m1,m2,m3,m4,m5,m6,m7,m8,f) = Cell.lift6 (fun (m1,m2,m3) m4 m5 m6 m7 m8 -> f(m1,m2,m3,m4,m5,m6,m7,m8)) (tuple3C m1 m2 m3,m4,m5,m6,m7,m8)
    member _.Bind9Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,f) = Cell.lift6 (fun (m1,m2,m3,m4) m5 m6 m7 m8 m9 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9)) (tuple4C m1 m2 m3 m4,m5,m6,m7,m8,m9)
    member _.Bind10Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,f) = Cell.lift6 (fun (m1,m2,m3,m4,m5) m6 m7 m8 m9 m10 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)) (tuple5C m1 m2 m3 m4 m5,m6,m7,m8,m9,m10)
    member _.Bind11Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,f) = Cell.lift6 (fun (m1,m2,m3,m4,m5,m6) m7 m8 m9 m10 m11 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)) (tuple6C m1 m2 m3 m4 m5 m6,m7,m8,m9,m10,m11)
    member _.Bind12Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,f) = Cell.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8) m9 m10 m11 m12 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)) (tuple6C m1 m2 m3 m4 m5 m6,tuple2C m7 m8,m9,m10,m11,m12)
    member _.Bind13Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,f) = Cell.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8,m9) m10 m11 m12 m13 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)) (tuple6C m1 m2 m3 m4 m5 m6,tuple3C m7 m8 m9,m10,m11,m12,m13)
    member _.Bind14Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,f) = Cell.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8,m9,m10) m11 m12 m13 m14 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)) (tuple6C m1 m2 m3 m4 m5 m6,tuple4C m7 m8 m9 m10,m11,m12,m13,m14)
    member _.Bind15Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,f) = Cell.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8,m9,m10,m11) m12 m13 m14 m15 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) (tuple6C m1 m2 m3 m4 m5 m6,tuple5C m7 m8 m9 m10 m11,m12,m13,m14,m15)
    member _.Bind16Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,f) = Cell.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8,m9,m10,m11,m12) m13 m14 m15 m16 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)) (tuple6C m1 m2 m3 m4 m5 m6,tuple6C m7 m8 m9 m10 m11 m12,m13,m14,m15,m16)
    
    member _.BindReturn(m,f) = Behavior.map f m
    member _.Bind2Return(m1,m2,f) = Behavior.lift2 (fun m1 m2 -> f(m1,m2)) (m1,m2)
    member _.Bind3Return(m1,m2,m3,f) = Behavior.lift3 (fun m1 m2 m3 -> f(m1,m2,m3)) (m1,m2,m3)
    member _.Bind4Return(m1,m2,m3,m4,f) = Behavior.lift4 (fun m1 m2 m3 m4 -> f(m1,m2,m3,m4)) (m1,m2,m3,m4)
    member _.Bind5Return(m1,m2,m3,m4,m5,f) = Behavior.lift5 (fun m1 m2 m3 m4 m5 -> f(m1,m2,m3,m4,m5)) (m1,m2,m3,m4,m5)
    member _.Bind6Return(m1,m2,m3,m4,m5,m6,f) = Behavior.lift6 (fun m1 m2 m3 m4 m5 m6 -> f(m1,m2,m3,m4,m5,m6)) (m1,m2,m3,m4,m5,m6)
    member _.Bind7Return(m1,m2,m3,m4,m5,m6,m7,f) = Behavior.lift6 (fun (m1,m2) m3 m4 m5 m6 m7 -> f(m1,m2,m3,m4,m5,m6,m7)) (tuple2B m1 m2,m3,m4,m5,m6,m7)
    member _.Bind8Return(m1,m2,m3,m4,m5,m6,m7,m8,f) = Behavior.lift6 (fun (m1,m2,m3) m4 m5 m6 m7 m8 -> f(m1,m2,m3,m4,m5,m6,m7,m8)) (tuple3B m1 m2 m3,m4,m5,m6,m7,m8)
    member _.Bind9Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,f) = Behavior.lift6 (fun (m1,m2,m3,m4) m5 m6 m7 m8 m9 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9)) (tuple4B m1 m2 m3 m4,m5,m6,m7,m8,m9)
    member _.Bind10Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,f) = Behavior.lift6 (fun (m1,m2,m3,m4,m5) m6 m7 m8 m9 m10 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)) (tuple5B m1 m2 m3 m4 m5,m6,m7,m8,m9,m10)
    member _.Bind11Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,f) = Behavior.lift6 (fun (m1,m2,m3,m4,m5,m6) m7 m8 m9 m10 m11 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)) (tuple6B m1 m2 m3 m4 m5 m6,m7,m8,m9,m10,m11)
    member _.Bind12Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,f) = Behavior.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8) m9 m10 m11 m12 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)) (tuple6B m1 m2 m3 m4 m5 m6,tuple2B m7 m8,m9,m10,m11,m12)
    member _.Bind13Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,f) = Behavior.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8,m9) m10 m11 m12 m13 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)) (tuple6B m1 m2 m3 m4 m5 m6,tuple3B m7 m8 m9,m10,m11,m12,m13)
    member _.Bind14Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,f) = Behavior.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8,m9,m10) m11 m12 m13 m14 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)) (tuple6B m1 m2 m3 m4 m5 m6,tuple4B m7 m8 m9 m10,m11,m12,m13,m14)
    member _.Bind15Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,f) = Behavior.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8,m9,m10,m11) m12 m13 m14 m15 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) (tuple6B m1 m2 m3 m4 m5 m6,tuple5B m7 m8 m9 m10 m11,m12,m13,m14,m15)
    member _.Bind16Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,f) = Behavior.lift6 (fun (m1,m2,m3,m4,m5,m6) (m7,m8,m9,m10,m11,m12) m13 m14 m15 m16 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)) (tuple6B m1 m2 m3 m4 m5 m6,tuple6B m7 m8 m9 m10 m11 m12,m13,m14,m15,m16)

let lift = LiftBuilder()

type SnapshotBuilder() =
    member _.Bind2Return(m1,m2,f) = Stream.snapshot m2 (fun m1 m2 -> f(m1,m2)) m1
    member _.Bind3Return(m1,m2,m3,f) = Stream.snapshot2 m2 m3 (fun m1 m2 m3 -> f(m1,m2,m3)) m1
    member _.Bind4Return(m1,m2,m3,m4,f) = Stream.snapshot3 m2 m3 m4 (fun m1 m2 m3 m4 -> f(m1,m2,m3,m4)) m1
    member _.Bind5Return(m1,m2,m3,m4,m5,f) = Stream.snapshot4 m2 m3 m4 m5 (fun m1 m2 m3 m4 m5 -> f(m1,m2,m3,m4,m5)) m1
    member _.Bind6Return(m1,m2,m3,m4,m5,m6,f) = Stream.snapshot5 m2 m3 m4 m5 m6 (fun m1 m2 m3 m4 m5 m6 -> f(m1,m2,m3,m4,m5,m6)) m1
    member _.Bind7Return(m1,m2,m3,m4,m5,m6,m7,f) = Stream.snapshot6 m2 m3 m4 m5 m6 m7 (fun m1 m2 m3 m4 m5 m6 m7 -> f(m1,m2,m3,m4,m5,m6,m7)) m1
    member _.Bind8Return(m1,m2,m3,m4,m5,m6,m7,m8,f) = Stream.snapshot7 m2 m3 m4 m5 m6 m7 m8 (fun m1 m2 m3 m4 m5 m6 m7 m8 -> f(m1,m2,m3,m4,m5,m6,m7,m8)) m1
    member x.Bind9Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,f) = Stream.snapshot8 m2 m3 m4 m5 m6 m7 m8 m9 (fun m1 m2 m3 m4 m5 m6 m7 m8 m9 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9)) m1
    member x.Bind10Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,f) = Stream.snapshot8 (Cell.lift2 (fun m2 m3 -> (m2,m3)) (m2,m3)) m4 m5 m6 m7 m8 m9 m10 (fun m1 (m2,m3) m4 m5 m6 m7 m8 m9 m10 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)) m1
    member x.Bind11Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,f) = Stream.snapshot8 (Cell.lift3 (fun m2 m3 m4 -> (m2,m3,m4)) (m2,m3,m4)) m5 m6 m7 m8 m9 m10 m11 (fun m1 (m2,m3,m4) m5 m6 m7 m8 m9 m10 m11 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)) m1
    member x.Bind12Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,f) = Stream.snapshot8 (Cell.lift4 (fun m2 m3 m4 m5 -> (m2,m3,m4,m5)) (m2,m3,m4,m5)) m6 m7 m8 m9 m10 m11 m12 (fun m1 (m2,m3,m4,m5) m6 m7 m8 m9 m10 m11 m12 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)) m1
    member x.Bind13Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,f) = Stream.snapshot8 (Cell.lift5 (fun m2 m3 m4 m5 m6 -> (m2,m3,m4,m5,m6)) (m2,m3,m4,m5,m6)) m7 m8 m9 m10 m11 m12 m13 (fun m1 (m2,m3,m4,m5,m6) m7 m8 m9 m10 m11 m12 m13 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)) m1
    member x.Bind14Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,f) = Stream.snapshot8 (Cell.lift6 (fun m2 m3 m4 m5 m6 m7 -> (m2,m3,m4,m5,m6,m7)) (m2,m3,m4,m5,m6,m7)) m8 m9 m10 m11 m12 m13 m14 (fun m1 (m2,m3,m4,m5,m6,m7) m8 m9 m10 m11 m12 m13 m14 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)) m1
    member x.Bind15Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,f) = Stream.snapshot8 (Cell.lift7 (fun m2 m3 m4 m5 m6 m7 m8 -> (m2,m3,m4,m5,m6,m7,m8)) (m2,m3,m4,m5,m6,m7,m8)) m9 m10 m11 m12 m13 m14 m15 (fun m1 (m2,m3,m4,m5,m6,m7,m8) m9 m10 m11 m12 m13 m14 m15 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) m1
    member x.Bind16Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,f) = Stream.snapshot8 (Cell.lift8 (fun m2 m3 m4 m5 m6 m7 m8 m9 -> (m2,m3,m4,m5,m6,m7,m8,m9)) (m2,m3,m4,m5,m6,m7,m8,m9)) m10 m11 m12 m13 m14 m15 m16 (fun m1 (m2,m3,m4,m5,m6,m7,m8,m9) m10 m11 m12 m13 m14 m15 m16 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)) m1
    
    member _.Bind2Return(m1,m2,f) = Stream.snapshotB m2 (fun m1 m2 -> f(m1,m2)) m1
    member _.Bind3Return(m1,m2,m3,f) = Stream.snapshot2B m2 m3 (fun m1 m2 m3 -> f(m1,m2,m3)) m1
    member _.Bind4Return(m1,m2,m3,m4,f) = Stream.snapshot3B m2 m3 m4 (fun m1 m2 m3 m4 -> f(m1,m2,m3,m4)) m1
    member _.Bind5Return(m1,m2,m3,m4,m5,f) = Stream.snapshot4B m2 m3 m4 m5 (fun m1 m2 m3 m4 m5 -> f(m1,m2,m3,m4,m5)) m1
    member _.Bind6Return(m1,m2,m3,m4,m5,m6,f) = Stream.snapshot5B m2 m3 m4 m5 m6 (fun m1 m2 m3 m4 m5 m6 -> f(m1,m2,m3,m4,m5,m6)) m1
    member _.Bind7Return(m1,m2,m3,m4,m5,m6,m7,f) = Stream.snapshot6B m2 m3 m4 m5 m6 m7 (fun m1 m2 m3 m4 m5 m6 m7 -> f(m1,m2,m3,m4,m5,m6,m7)) m1
    member _.Bind8Return(m1,m2,m3,m4,m5,m6,m7,m8,f) = Stream.snapshot7B m2 m3 m4 m5 m6 m7 m8 (fun m1 m2 m3 m4 m5 m6 m7 m8 -> f(m1,m2,m3,m4,m5,m6,m7,m8)) m1
    member x.Bind9Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,f) = Stream.snapshot8B m2 m3 m4 m5 m6 m7 m8 m9 (fun m1 m2 m3 m4 m5 m6 m7 m8 m9 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9)) m1
    member x.Bind10Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,f) = Stream.snapshot8B (Behavior.lift2 (fun m2 m3 -> (m2,m3)) (m2,m3)) m4 m5 m6 m7 m8 m9 m10 (fun m1 (m2,m3) m4 m5 m6 m7 m8 m9 m10 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)) m1
    member x.Bind11Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,f) = Stream.snapshot8B (Behavior.lift3 (fun m2 m3 m4 -> (m2,m3,m4)) (m2,m3,m4)) m5 m6 m7 m8 m9 m10 m11 (fun m1 (m2,m3,m4) m5 m6 m7 m8 m9 m10 m11 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)) m1
    member x.Bind12Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,f) = Stream.snapshot8B (Behavior.lift4 (fun m2 m3 m4 m5 -> (m2,m3,m4,m5)) (m2,m3,m4,m5)) m6 m7 m8 m9 m10 m11 m12 (fun m1 (m2,m3,m4,m5) m6 m7 m8 m9 m10 m11 m12 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)) m1
    member x.Bind13Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,f) = Stream.snapshot8B (Behavior.lift5 (fun m2 m3 m4 m5 m6 -> (m2,m3,m4,m5,m6)) (m2,m3,m4,m5,m6)) m7 m8 m9 m10 m11 m12 m13 (fun m1 (m2,m3,m4,m5,m6) m7 m8 m9 m10 m11 m12 m13 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)) m1
    member x.Bind14Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,f) = Stream.snapshot8B (Behavior.lift6 (fun m2 m3 m4 m5 m6 m7 -> (m2,m3,m4,m5,m6,m7)) (m2,m3,m4,m5,m6,m7)) m8 m9 m10 m11 m12 m13 m14 (fun m1 (m2,m3,m4,m5,m6,m7) m8 m9 m10 m11 m12 m13 m14 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)) m1
    member x.Bind15Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,f) = Stream.snapshot8B (Behavior.lift7 (fun m2 m3 m4 m5 m6 m7 m8 -> (m2,m3,m4,m5,m6,m7,m8)) (m2,m3,m4,m5,m6,m7,m8)) m9 m10 m11 m12 m13 m14 m15 (fun m1 (m2,m3,m4,m5,m6,m7,m8) m9 m10 m11 m12 m13 m14 m15 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) m1
    member x.Bind16Return(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,f) = Stream.snapshot8B (Behavior.lift8 (fun m2 m3 m4 m5 m6 m7 m8 m9 -> (m2,m3,m4,m5,m6,m7,m8,m9)) (m2,m3,m4,m5,m6,m7,m8,m9)) m10 m11 m12 m13 m14 m15 m16 (fun m1 (m2,m3,m4,m5,m6,m7,m8,m9) m10 m11 m12 m13 m14 m15 m16 -> f(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)) m1

let snapshot = SnapshotBuilder()