module SodiumFRP.MutableListener

open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let setListener l (m : MutableListener) = m.SetListenerImpl l

[<MethodImpl(MethodImplOptions.NoInlining)>]
let clearListener (m : MutableListener) = m.ClearListenerImpl ()

let unlisten (m : MutableListener) = Listener.unlisten m
let getListenerWithWeakReference (m : MutableListener) = Listener.getListenerWithWeakReference m