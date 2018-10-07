

namespace SodiumFRP.FSharp
  module Lazy = begin
    val map : f:('T -> 'a) -> l:Lazy<'T> -> Lazy<'a>
    val lift2 : f:('T1 -> 'T2 -> 'a) -> l1:Lazy<'T1> -> l2:Lazy<'T2> -> Lazy<'a>
    val lift3 :
      f:('T1 -> 'T2 -> 'T3 -> 'a) ->
        l1:Lazy<'T1> -> l2:Lazy<'T2> -> l3:Lazy<'T3> -> Lazy<'a>
    val lift4 :
      f:('T1 -> 'T2 -> 'T3 -> 'T4 -> 'a) ->
        l1:Lazy<'T1> -> l2:Lazy<'T2> -> l3:Lazy<'T3> -> l4:Lazy<'T4> -> Lazy<'a>
    val lift5 :
      f:('T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'a) ->
        l1:Lazy<'T1> ->
          l2:Lazy<'T2> ->
            l3:Lazy<'T3> -> l4:Lazy<'T4> -> l5:Lazy<'T5> -> Lazy<'a>
    val lift6 :
      f:('T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'a) ->
        l1:Lazy<'T1> ->
          l2:Lazy<'T2> ->
            l3:Lazy<'T3> ->
              l4:Lazy<'T4> -> l5:Lazy<'T5> -> l6:Lazy<'T6> -> Lazy<'a>
    val lift7 :
      f:('T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'T7 -> 'a) ->
        l1:Lazy<'T1> ->
          l2:Lazy<'T2> ->
            l3:Lazy<'T3> ->
              l4:Lazy<'T4> ->
                l5:Lazy<'T5> -> l6:Lazy<'T6> -> l7:Lazy<'T7> -> Lazy<'a>
    val lift8 :
      f:('T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'T7 -> 'T8 -> 'a) ->
        l1:Lazy<'T1> ->
          l2:Lazy<'T2> ->
            l3:Lazy<'T3> ->
              l4:Lazy<'T4> ->
                l5:Lazy<'T5> ->
                  l6:Lazy<'T6> -> l7:Lazy<'T7> -> l8:Lazy<'T8> -> Lazy<'a>
  end

namespace SodiumFRP.FSharp
  type Transaction =
    class
    end
  module Transaction = begin
    val isActive : unit -> bool
    val run : f:(unit -> 'a) -> 'a
    val onStart : a:(unit -> unit) -> unit
    val post : a:(unit -> unit) -> unit
  end

namespace SodiumFRP.FSharp
  type IListenerWithWeakReference =
    interface
      abstract member Unlisten : unit -> unit
    end
  type IListener =
    interface
      abstract member
        GetListenerWithWeakReference : unit -> IListenerWithWeakReference
      abstract member Unlisten : unit -> unit
    end
  type IStrongListener =
    interface
      inherit System.IDisposable
      inherit IListener
    end
  type IWeakListener =
    interface
      inherit IListener
    end
  module Listener = begin
    val unlistenWeak : listener:IListenerWithWeakReference -> unit
    val unlisten : listener:IListener -> unit
    val getListenerWithWeakReference :
      listener:IListener -> IListenerWithWeakReference
    val empty : IListener
    val fromList : listeners:IListener list -> IListener
    val fromWeakList : listeners:IWeakListener list -> IWeakListener
    val fromStrongList : listeners:IStrongListener list -> IStrongListener
    val fromSeq : listeners:seq<IListener> -> IListener
    val append : l1:IListener -> l2:IListener -> IListener
  end
