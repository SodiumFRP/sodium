module Sodium.Frp.Transaction

open System
open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let isActive () = TransactionInternal.IsActiveImpl ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let run f = TransactionInternal.RunImpl (Func<_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let onStart a = TransactionInternal.OnStartImpl (Action a)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let post a = TransactionInternal.PostImpl (Action a)