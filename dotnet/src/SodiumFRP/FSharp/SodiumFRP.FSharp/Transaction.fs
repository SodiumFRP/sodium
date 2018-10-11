module SodiumFRP.Transaction

open System

let isActive () = TransactionInternal.IsActiveImpl ()
let run f = TransactionInternal.RunImpl (Func<_> f)
let onStart a = TransactionInternal.OnStartImpl (Action a)
let post a = TransactionInternal.PostImpl (Action a)