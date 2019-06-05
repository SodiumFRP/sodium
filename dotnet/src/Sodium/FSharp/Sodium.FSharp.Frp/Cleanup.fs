module Sodium.Frp.Cleanup

open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let cleanupNow (cleanup : Cleanup) = cleanup.CleanupNowImpl ()