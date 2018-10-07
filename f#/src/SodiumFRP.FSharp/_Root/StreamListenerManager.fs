namespace SodiumFRP.FSharp

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
#if !(NETSTANDARD2_0 || NET45)
open System.Threading.Tasks
#endif

module internal StreamListenerManagerPrivate =
    let StreamsByIdLock = obj()

type internal StreamListeners (streamId : Guid) as this =
    let listeners = List<IListenerWithWeakReference>()
    static let mutable streamsById = Dictionary<Guid, StreamListeners>()
    static let mutable streamsByIdCapacity = 0
    do
        lock StreamListenerManagerPrivate.StreamsByIdLock (fun () ->
            streamsById.Add(streamId, this)
            streamsByIdCapacity <- streamsByIdCapacity + 1)
    static member TryGetStreamListenersById streamId =
        let (found, streamListeners) = streamsById.TryGetValue streamId
        if found then Some streamListeners else None
    member __.Unlisten () =
        listeners.ForEach (fun l -> l.Unlisten ())
        lock StreamListenerManagerPrivate.StreamsByIdLock (fun () ->
            streamsById.Remove streamId |> ignore
            if streamsByIdCapacity > 100 && streamsById.Count < streamsByIdCapacity / 2 then
                streamsById <- Dictionary<Guid, StreamListeners>(streamsById))
    member __.AddListener listener = listeners.Add listener

module internal StreamListenerManager =

    let private streamIdsToRemove = new BlockingCollection<Guid>()
    let private streamIdsToRemoveLastChance = new ConcurrentQueue<Guid>()
    let remove streamId = streamIdsToRemove.Add streamId
    do
        let sodiumCleanup () =
            for streamId in streamIdsToRemove.GetConsumingEnumerable () do
                let streamListeners = lock StreamListenerManagerPrivate.StreamsByIdLock (fun () -> StreamListeners.TryGetStreamListenersById streamId)
                match streamListeners with | Some l -> l.Unlisten () | None -> streamIdsToRemoveLastChance.Enqueue streamId
        
        let sodiumLastChanceCleanup () =
            while true do
                Thread.Sleep 30000
                let (foundLocal, streamIdLocal) = streamIdsToRemoveLastChance.TryDequeue ()
                let mutable found = foundLocal
                let mutable streamId = streamIdLocal
                while found do
                    let streamListeners = lock StreamListenerManagerPrivate.StreamsByIdLock (fun () -> StreamListeners.TryGetStreamListenersById streamId)
                    streamListeners |> Option.iter (fun l -> l.Unlisten ())
                    let (foundLocal, streamIdLocal) = streamIdsToRemoveLastChance.TryDequeue ()
                    found <- foundLocal
                    streamId <- streamIdLocal
        
#if NETSTANDARD2_0 || NET45
        let cleanupThread = Thread(sodiumCleanup)
        cleanupThread.Name <- "Sodium Cleanup Thread"
        cleanupThread.IsBackground <- true
        cleanupThread.Start ()
        
        let lastChanceCleanupThread = Thread(sodiumLastChanceCleanup)
        lastChanceCleanupThread.Name <- "Sodium Last Chance Cleanup Thread"
        lastChanceCleanupThread.IsBackground <- true
        lastChanceCleanupThread.Start ()
#else
        Task.Factory.StartNew(sodiumCleanup, TaskCreationOptions.LongRunning ||| TaskCreationOptions.DenyChildAttach) |> ignore
        Task.Factory.StartNew(sodiumLastChanceCleanup, TaskCreationOptions.LongRunning ||| TaskCreationOptions.DenyChildAttach) |> ignore
#endif