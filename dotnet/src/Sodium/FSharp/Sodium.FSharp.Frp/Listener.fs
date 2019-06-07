namespace Sodium.Frp

module Listener =
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

    let append l1 l2 = fromList (l1::l2::[])

module WeakListener =
    open System.Runtime.CompilerServices

    let unlisten (listener : IWeakListener) = listener.Unlisten ()
    let getListenerWithWeakReference (listener : IWeakListener) = listener.GetListenerWithWeakReference ()

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let empty = ListenerInternal.EmptyWeakImpl

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let fromList listeners = ListenerInternal.CreateWeakCompositeImpl listeners

    let fromSeq listeners = List.ofSeq listeners |> fromList

    let append l1 l2 = fromList (l1::l2::[])

module StrongListener =
    open System.Runtime.CompilerServices

    let unlisten (listener : IStrongListener) = listener.Unlisten ()
    let getListenerWithWeakReference (listener : IStrongListener) = listener.GetListenerWithWeakReference ()

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let empty = ListenerInternal.EmptyStrongImpl

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let fromList listeners = ListenerInternal.CreateStrongCompositeImpl listeners

    let fromSeq listeners = List.ofSeq listeners |> fromList

    let append l1 l2 = fromList (l1::l2::[])

module MutableListener =
    open System.Runtime.CompilerServices

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let setListener l (m : MutableListener) = m.SetListenerImpl l

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let clearListener (m : MutableListener) = m.ClearListenerImpl ()

    let unlisten (m : MutableListener) = Listener.unlisten m
    let getListenerWithWeakReference (m : MutableListener) = Listener.getListenerWithWeakReference m