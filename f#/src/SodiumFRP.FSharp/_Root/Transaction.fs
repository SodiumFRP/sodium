namespace rec SodiumFRP.FSharp

open System
open System.Collections.Generic
open System.Linq
open System.Runtime.ExceptionServices
open System.Threading
open Priority_Queue

module internal TransactionPrivate =
    let localTransaction = new ThreadLocal<Transaction option>(fun () -> None)
    let transactionLock = obj()
    let onStartHooks = List<unit -> unit>()

type internal ApplyResult<'T> = | Result of 'T | Exception of exn
type internal TransactionDelegate = delegate of Transaction -> unit

type Transaction =
    [<DefaultValue>] static val mutable private runningOnStartHooks : bool
    val mutable private isElevated : bool
    val mutable private entries : List<Entry>
    val private sendQueue : List<Transaction -> unit>
    val mutable private sampleQueue : List<unit -> unit>
    val private lastQueue : Queue<unit -> unit>
    val private postQueue : Queue<Transaction -> unit>
    val mutable private splitQueue : Dictionary<int, Transaction -> unit>
    val private hasParentTransaction : bool
    val mutable internal targetsToActivate : List<Target>
    val mutable internal activatedTargets : bool
    val private prioritizedQueue : SimplePriorityQueue<Entry, int64>
    val mutable private toRegen : bool
    
    private new
        (
            postQueue : Queue<Transaction -> unit>,
            splitQueue : Dictionary<int, Transaction -> unit>,
            hasParentTransaction : bool) =
        {
            isElevated = false;
            entries = List<Entry>();
            sendQueue = List<Transaction -> unit>();
            sampleQueue = List<unit -> unit>();
            lastQueue = Queue<unit -> unit>();
            postQueue = postQueue;
            splitQueue = splitQueue;
            hasParentTransaction = hasParentTransaction;
            targetsToActivate = List<Target>();
            activatedTargets = false;
            prioritizedQueue = SimplePriorityQueue<Entry, int64>();
            toRegen = false
        }
    
    internal new() = Transaction(Queue<Transaction -> unit>(), Dictionary<int, Transaction -> unit>(), false)
    
    static member internal HasCurrentTransaction () = TransactionPrivate.localTransaction.Value.IsSome
    static member internal GetCurrentTransaction () = TransactionPrivate.localTransaction.Value
    
    static member private RunStartHooks () =
        if not Transaction.runningOnStartHooks then
            Transaction.runningOnStartHooks <- true
            try for action in TransactionPrivate.onStartHooks do action ()
            finally Transaction.runningOnStartHooks <- false
    
    static member private EnsureElevated (t : Transaction) =
        if not t.isElevated then
            t.isElevated <- true
            if not t.hasParentTransaction then
                Monitor.Enter TransactionPrivate.transactionLock
            Transaction.RunStartHooks ()
    
    static member internal Apply (code : Transaction -> bool -> 'T) (ensureElevated : bool) =
        let transaction = TransactionPrivate.localTransaction.Value
        let createdNewTransaction = transaction.IsNone
        let (result, newTransaction) =
            try
                let newTransaction = transaction |> Option.defaultWith Transaction
                if createdNewTransaction then TransactionPrivate.localTransaction.Value <- Some newTransaction
                if ensureElevated then Transaction.EnsureElevated newTransaction
                (Result (code newTransaction createdNewTransaction), Some newTransaction)
            with | e -> (Exception e, None)
        
        try
            try if createdNewTransaction then newTransaction |> Option.iter (fun t -> t.Close ())
            with
                | e2 ->
                    match result with
                        | Result _ -> reraise ()
                        | Exception e -> AggregateException(e, e2) |> raise
            
            match result with
                | Result r -> r
                | Exception e ->
                    (ExceptionDispatchInfo.Capture e).Throw ()
                    raise e
        finally
            if createdNewTransaction then
                newTransaction |> Option.iter (fun t ->
                    if t.isElevated && not t.hasParentTransaction then
                        Monitor.Exit TransactionPrivate.transactionLock)
                TransactionPrivate.localTransaction.Value <- None
    
    member internal this.Send a = this.sendQueue.Add a
    
    member internal this.Prioritized (node : Node) a =
        let e = Entry(node, a)
        lock Node.NodeRanksLock (fun () -> this.prioritizedQueue.Enqueue(e, node.Rank))
        this.entries.Add e
    
    member internal this.Sample a = this.sampleQueue.Add a
    
    member internal this.Last a = this.lastQueue.Enqueue a
    
    member internal this.Post a = this.postQueue.Enqueue a
    
    member internal this.Split index a =
        let found, existing = this.splitQueue.TryGetValue index
        let n = if found then Delegate.Combine(TransactionDelegate(existing), TransactionDelegate(a)) :?> TransactionDelegate else TransactionDelegate(a)
        this.splitQueue.[index] <- n.Invoke
    
    member internal this.SetNeedsRegenerating () = this.toRegen <- true
    
    member private this.CheckRegen () =
        if this.toRegen then
            this.toRegen <- false
            this.prioritizedQueue.Clear ()
            lock Node.NodeRanksLock (fun () ->
                let newEntries = List<Entry>(this.entries.Count)
                for e in this.entries do
                    if not e.IsRemoved then
                        newEntries.Add e
                        this.prioritizedQueue.Enqueue(e, e.Node.Rank)
                this.entries <- newEntries)
    
    member private this.Close () =
        Transaction.EnsureElevated this
        for target in this.targetsToActivate do target.IsActivated <- true
        this.activatedTargets <- true
        for i = 0 to this.sendQueue.Count - 1 do this.sendQueue.[i] this
        this.sendQueue.Clear ()
        while this.prioritizedQueue.Count > 0 || this.sampleQueue.Count > 0 do
            while this.prioritizedQueue.Count > 0 do
                this.CheckRegen ()
                let e = this.prioritizedQueue.Dequeue ()
                e.IsRemoved <- true
                e.Action this
            let sq = this.sampleQueue
            this.sampleQueue <- List<unit -> unit>()
            for s in sq do s ()
        while this.lastQueue.Count > 0 do this.lastQueue.Dequeue () ()
        if not this.hasParentTransaction then
            let executeInNewTransaction a runStartHooks =
                try
                    let transaction = Transaction(this.postQueue, this.splitQueue, true)
                    if not runStartHooks then transaction.isElevated <- true
                    TransactionPrivate.localTransaction.Value <- Some transaction
                    try a transaction finally transaction.Close ()
                finally TransactionPrivate.localTransaction.Value <- Some this
            
            while this.postQueue.Count > 0 || this.splitQueue.Count > 0 do
                while this.postQueue.Count > 0 do executeInNewTransaction (this.postQueue.Dequeue ()) true
                let sq = this.splitQueue
                this.splitQueue <- Dictionary<int, Transaction -> unit>()
                for n in sq.Keys.OrderBy(fun v -> v) do executeInNewTransaction sq.[n] false

type internal Entry =
    val public Node : Node
    val public Action : Transaction -> unit
    val mutable public IsRemoved : bool
    new(node, a) = { Node = node; Action = a; IsRemoved = false }

type [<AbstractClass>] internal Node(rank : int64) =
    static let listenersLock = obj()
    
    static let rec ensureBiggerThan (node : Node) limit =
        let rec ensureBiggerThan (originalNode : Node) (node : Node) limit =
            if LanguagePrimitives.PhysicalEquality originalNode node then failwith "A dependency cycle was detected."
            if node.Rank <= limit then
                node.Rank <- (limit + 1L)
                for n in node.GetListenerNodesUnsafe () do ensureBiggerThan originalNode n node.Rank
        if node.Rank > limit then false
        else
            node.Rank <- limit + 1L
            lock listenersLock (fun () ->
                for n in node.GetListenerNodesUnsafe () do ensureBiggerThan node n node.Rank)
            true

    static member val Null = Node<_>(Int64.MaxValue)
    
    static member ListenersLock = listenersLock
    static member val NodeRanksLock = obj()
    static member EnsureBiggerThan node limit = ensureBiggerThan node limit

    member val Rank = rank with get,set

    abstract member GetListenerNodesUnsafe : unit -> Node list

type [<AbstractClass>] internal Target =
    val public Node : Node
    val mutable public IsActivated : bool
    new(node, isActivated) = { Node = node; IsActivated = isActivated }

type internal Node<'T> internal (rank : int64) =
    inherit Node(rank)

    let mutable listeners = HashSet<_>()
    let mutable listenersCapacity = 0
    
    internal new() = Node<_>(0L)

    member this.Link (transaction : Transaction) a target =
        let t = Target<_>(a, target, transaction.activatedTargets)
        if not transaction.activatedTargets then transaction.targetsToActivate.Add t
        lock Node.ListenersLock (fun () ->
            listeners.Add t |> ignore
            listenersCapacity <- listenersCapacity + 1)
        let changed = lock Node.NodeRanksLock (fun () -> Node.EnsureBiggerThan target this.Rank)
        (changed, t)

    member this.Unlink target = this.RemoveListener target
    
    member __.GetListenersCopy () = lock Node.ListenersLock (fun () -> List.ofSeq listeners)
    
    member __.RemoveListener l =
        lock Node.ListenersLock (fun () ->
            listeners.Remove l |> ignore
            if listenersCapacity > 100 && listeners.Count < listenersCapacity / 2 then
                listeners <- HashSet<_>(listeners)
                listenersCapacity <- listeners.Count)

    override __.GetListenerNodesUnsafe () = Seq.map (fun (t : Target<'T>) -> t.Node) listeners |> List.ofSeq

type internal Target<'T> =
    inherit Target
    val public Action : WeakReference<Transaction -> 'T -> unit>
    new (a, node, isActivated) =
        { inherit Target(node, isActivated); Action = new WeakReference<Transaction -> 'T -> unit>(a) }

module Transaction =
    let isActive () = Transaction.HasCurrentTransaction()
    
    let run f = Transaction.Apply (fun _ _ -> f ()) false
    
    let onStart a = lock TransactionPrivate.transactionLock (fun () -> TransactionPrivate.onStartHooks.Add a)
    
    let post a =
        let applyPost (t : Transaction) createdNewTransaction =
            if createdNewTransaction then a () else t.Post (fun _ -> a ())
        Transaction.Apply applyPost false
    
    let internal prioritized node a (transaction : Transaction) = transaction.Prioritized node a
