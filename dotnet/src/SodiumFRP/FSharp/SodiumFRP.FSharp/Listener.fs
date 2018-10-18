module SodiumFRP.Listener

open System.Runtime.CompilerServices

let unlistenWeak (listener : IListenerWithWeakReference) = listener.Unlisten ()
let unlisten (listener : IListener) = listener.Unlisten ()
let getListenerWithWeakReference (listener : IListener) = listener.GetListenerWithWeakReference ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let empty = ListenerInternal.EmptyImpl

[<MethodImpl(MethodImplOptions.NoInlining)>]
let fromList listeners = ListenerInternal.CreateCompositeImpl listeners

[<MethodImpl(MethodImplOptions.NoInlining)>]
let fromWeakList listeners = ListenerInternal.CreateWeakCompositeImpl listeners

[<MethodImpl(MethodImplOptions.NoInlining)>]
let fromStrongList listeners = ListenerInternal.CreateStrongCompositeImpl listeners

let fromSeq listeners = List.ofSeq listeners |> fromList
let fromWeakSeq listeners = List.ofSeq listeners |> fromWeakList
let fromStrongSeq listeners = List.ofSeq listeners |> fromStrongList

[<MethodImpl(MethodImplOptions.NoInlining)>]
let append l1 l2 = ListenerInternal.AppendImpl (l1, l2)