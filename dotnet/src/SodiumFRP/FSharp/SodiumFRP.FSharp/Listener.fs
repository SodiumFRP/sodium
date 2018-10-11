module SodiumFRP.Listener

let unlistenWeak (listener : IListenerWithWeakReference) = listener.Unlisten ()
let unlisten (listener : IListener) = listener.Unlisten ()
let getListenerWithWeakReference (listener : IListener) = listener.GetListenerWithWeakReference ()
let empty = ListenerInternal.EmptyImpl
let fromList listeners = ListenerInternal.CreateCompositeImpl listeners
let fromWeakList listeners = ListenerInternal.CreateWeakCompositeImpl listeners
let fromStrongList listeners = ListenerInternal.CreateStrongCompositeImpl listeners
let fromSeq listeners = List.ofSeq listeners |> fromList
let append l1 l2 = ListenerInternal.AppendImpl (l1, l2)