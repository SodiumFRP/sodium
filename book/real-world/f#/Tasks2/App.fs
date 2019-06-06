module Tasks.App

open System
open System.Threading
open SodiumFRP

type IExample =
    abstract Name : string
    abstract Run : unit -> Async<unit>

type Promise1 () =
    interface IExample with
        member __.Name = "Promise 1"
        member __.Run () =
            async {
                printfn "*** test"
                do!
                    async {
                        let s = sinkS ()
                        let r' = s |> listenOnceAsyncS
                        s |> sendS "Sent"
                        do! Async.Sleep 500
                        printfn "Before await"
                        let! r = r'
                        printfn "%s" r
                        printfn "After await"
                    }
            }

type Promise1WithThreads () =
    interface IExample with
        member __.Name = "Promise 1 With Threads"
        member __.Run () =
            async {
                printfn "*** test 1"
                do!
                    async {
                        let s = sinkS ()
                        let r' = s |> listenOnceAsyncS
                        Thread(fun () ->
                            Thread.Sleep 500
                            s |> sendS "Sent").Start()
                        let! r = r'
                        printfn "%s" r
                    }
                printfn ""
                printfn "*** test 2"
                do!
                    async {
                        let s = sinkS ()
                        let r' = s |> listenOnceAsyncS
                        Thread(fun () -> s |> sendS "Sent").Start()
                        do! Async.Sleep 500
                        let! r = r'
                        printfn "%s" r
                    }
            }

type Promise2 () =
    interface IExample with
        member __.Name = "Promise 2"
        member __.Run () =
            async {
                printfn "*** Simple test"
                do!
                    async {
                        let sa = sinkS ()
                        let sb = sinkS ()
                        let ra' = sa |> listenOnceAsyncS
                        let rb' = sb |> listenOnceAsyncS
                        sa |> sendS "Hello"
                        sb |> sendS "World"
                        let! ra = ra'
                        let! rb = rb'
                        printfn "%s %s" ra rb
                    }
                printfn ""
                printfn "*** Simultaneous case"
                do!
                    async {
                        let sa = sinkS ()
                        let sb = sinkS ()
                        let ra' = sa |> listenOnceAsyncS
                        let rb' = sb |> listenOnceAsyncS
                        runT (fun () ->
                            sa |> sendS "Hello"
                            sb |> sendS "World")
                        let! ra = ra'
                        let! rb = rb'
                        printfn "%s %s" ra rb
                    }
            }

type Promise2WithThreads () =
    interface IExample with
        member __.Name = "Promise 2 With Threads"
        member __.Run () =
            async {
                printfn "*** Simple test 1"
                do!
                    async {
                        let sa = sinkS ()
                        let sb = sinkS ()
                        let ra' = sa |> listenOnceAsyncS
                        let rb' = sb |> listenOnceAsyncS
                        Thread(fun () ->
                            Thread.Sleep 500
                            sa |> sendS "Hello"
                            sb |> sendS "World").Start()
                        let! ra = ra'
                        let! rb = rb'
                        printfn "%s %s" ra rb
                    }
                printfn ""
                printfn "*** Simple test 2"
                do!
                    async {
                        let sa = sinkS ()
                        let sb = sinkS ()
                        let ra' = sa |> listenOnceAsyncS
                        let rb' = sb |> listenOnceAsyncS
                        Thread(fun () ->
                            sa |> sendS "Hello"
                            sb |> sendS "World").Start()
                        do! Async.Sleep 500
                        let! ra = ra'
                        let! rb = rb'
                        printfn "%s %s" ra rb
                    }
                printfn ""
                printfn "*** Simultaneous case 1"
                do!
                    async {
                        let sa = sinkS ()
                        let sb = sinkS ()
                        let ra' = sa |> listenOnceAsyncS
                        let rb' = sb |> listenOnceAsyncS
                        Thread(fun () ->
                            Thread.Sleep 500
                            runT (fun () ->
                                sa |> sendS "Hello"
                                sb |> sendS "World")).Start()
                        let! ra = ra'
                        let! rb = rb'
                        printfn "%s %s" ra rb
                    }
                printfn ""
                printfn "*** Simultaneous case 2"
                do!
                    async {
                        let sa = sinkS ()
                        let sb = sinkS ()
                        let ra' = sa |> listenOnceAsyncS
                        let rb' = sb |> listenOnceAsyncS
                        Thread(fun () ->
                            runT (fun () ->
                                sa |> sendS "Hello"
                                sb |> sendS "World")).Start()
                        do! Async.Sleep 500
                        let! ra = ra'
                        let! rb = rb'
                        printfn "%s %s" ra rb
                    }
            }
            
let run () =
    async {
        let actions =
            let create name (example : IExample) = (name, example)
            Map.ofArray
                [|
                    Promise1 () |> create 'a';
                    Promise1WithThreads () |> create 'b';
                    Promise2 () |> create 'c';
                    Promise2WithThreads () |> create 'd'
                |]
        for p in actions do printfn "(%s) %s" (string p.Key) p.Value.Name
        
        let rec loop () =
            async {
                printfn ""
                printf "Select an example to run: "
                let c = Console.ReadKey ()
                do!
                    match actions |> Map.tryFind c.KeyChar with
                    | Some example ->
                        async {
                            printfn ""
                            printfn ""
                            do! example.Run ()
                        }
                    | None ->
                        async {
                            printfn ""
                            printfn "Invalid selection."
                            return! loop ()
                        }
            }
        do! loop ()
    }