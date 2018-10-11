module SodiumFRP.MutableListener

let setListener l (m : MutableListener) = m.SetListenerImpl l
let clearListener (m : MutableListener) = m.ClearListenerImpl ()
let unlisten (m : MutableListener) = Listener.unlisten m
let getListenerWithWeakReference (m : MutableListener) = Listener.getListenerWithWeakReference m