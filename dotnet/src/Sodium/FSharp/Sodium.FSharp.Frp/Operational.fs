module Sodium.Frp.Operational

open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let updates behavior = OperationalInternal.UpdatesImpl behavior

[<MethodImpl(MethodImplOptions.NoInlining)>]
let value behavior = OperationalInternal.ValueImpl behavior

[<MethodImpl(MethodImplOptions.NoInlining)>]
let split (stream : Stream<#seq<_>>) = OperationalInternal.SplitImpl stream

[<MethodImpl(MethodImplOptions.NoInlining)>]
let defer stream = OperationalInternal.DeferImpl stream