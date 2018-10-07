namespace SodiumFRP.FSharp

type internal WeakMutableListener =
    val mutable public WeakListener : IListenerWithWeakReference option
    new() = { WeakListener = None }
    interface IListenerWithWeakReference with
        member this.Unlisten() =
            this.WeakListener |> Option.iter Listener.unlistenWeak

type MutableListener() =
    let weakMutableListener = WeakMutableListener()
    let mutable listener = None
    member internal __.SetListener l =
        listener <- Some l
        weakMutableListener.WeakListener <- Some <| Listener.getListenerWithWeakReference l
    member internal __.ClearListener () =
        listener <- None
        weakMutableListener.WeakListener <- None
    interface IListener with
        member __.Unlisten() =
            listener |> Option.iter Listener.unlisten
        member __.GetListenerWithWeakReference () = upcast weakMutableListener

module MutableListener =
    let setListener l (m : MutableListener) = m.SetListener l
    let clearListener (m : MutableListener) = m.ClearListener ()
    let unlisten (m : MutableListener) = Listener.unlisten m
    let getListenerWithWeakReference (m : MutableListener) = Listener.getListenerWithWeakReference m
