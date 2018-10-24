module Fridgets.Supply

type private Impl() =
    let mutable nextId = 0L
    let nextIdLock = obj()

    member __.Alloc () = lock nextIdLock (fun () ->
        nextId <- nextId + 1L
        nextId)

type T = private
            {
                impl : Impl
                mutable id' : int64 option
                idLock : obj
                mutable child1 : T option
                child1Lock : obj
                mutable child2 : T option
                child2Lock : obj
            }

let private getImpl l v s alloc =
    lock l (fun () ->
        let (v, action) =
            match v with
                | None ->
                    let v = alloc()
                    (v, (fun () -> s (Some v)))
                | Some v -> (v, id)
        action ()
        v)

let create () =
    {
        impl = Impl()
        id' = None
        idLock = obj()
        child1 = None
        child1Lock = obj()
        child2 = None
        child2Lock = obj()
    }

let private createWithImpl impl = { create () with impl = impl }

let get s = getImpl s.idLock s.id' (fun v -> s.id' <- v) s.impl.Alloc
let child1 s = getImpl s.child1Lock s.child1 (fun v -> s.child1 <- v) (fun () -> createWithImpl s.impl)
let child2 s = getImpl s.child2Lock s.child2 (fun v -> s.child2 <- v) (fun () -> createWithImpl s.impl)