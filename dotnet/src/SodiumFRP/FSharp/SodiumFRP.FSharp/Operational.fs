module SodiumFRP.Operational

let updates behavior = OperationalInternal.UpdatesImpl behavior
let value behavior = OperationalInternal.ValueImpl behavior
let split (stream : Stream<#seq<_>>) = OperationalInternal.SplitImpl stream
let defer stream = OperationalInternal.DeferImpl stream