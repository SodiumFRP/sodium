module SodiumFRP.Cleanup

let cleanupNow (cleanup : Cleanup) = cleanup.CleanupNowImpl ()