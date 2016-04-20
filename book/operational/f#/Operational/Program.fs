open System
open Sodium

type Example = { name : string; run : unit -> unit }

[<EntryPoint>]
let main _ =
    let cell () =
        let x = Cell.sink 0
        use _l = x |> Cell.listen (printfn "%i")
        x.Send 10
        x.Send 20
        x.Send 30

    let sameTransaction () =
        let sX = Stream.sink ()
        let sXPlus1 = sX |> Stream.map ((+) 1)
        use _l = Transaction.Run (fun () ->
            sX.Send 1
            sXPlus1 |> Stream.listen (printfn "%i"))
        sX.Send 2
        sX.Send 3

    let sendInCallback () =
        let sX = Stream.sink ()
        let sY = Stream.sink ()
        use _l =
            Listener.fromSeq
                [
                    sX |> Stream.listen sY.Send
                    sY |> Stream.listen (printfn "%i")
                ]
        sX.Send 1
        sX.Send 2
        sX.Send 3

    let split () =
        let ``as`` = Stream.sink ()
        use _l = ``as`` |> Operational.split |> Stream.accum (+) 0 |> Operational.updates |> Stream.listen (printfn "%i")
        ``as``.Send [ 100; 15; 60 ]
        ``as``.Send [ 1; 5 ]

    let stream () =
        let sX = Stream.sink ()
        let sXPlus1 = sX |> Stream.map ((+) 1)
        use _l = sXPlus1 |> Stream.listen (printfn "%i")
        sX.Send 1
        sX.Send 2
        sX.Send 3

    let updates () =
        let x = Cell.sink 0
        x.Send 1
        use _l = x |> Operational.updates |> Stream.listen (printfn "%i")
        x.Send 2
        x.Send 3

    let value1 () =
        let x = Cell.sink 0
        x.Send 1
        use _l = x |> Operational.value |> Stream.listen (printfn "%i")
        x.Send 2
        x.Send 3

    let value2 () =
        let x = Cell.sink 0
        x.Send 1
        use _l = Transaction.Run (fun () -> x |> Operational.value |> Stream.listen (printfn "%i"))
        x.Send 2
        x.Send 3

    let cellExample = { name = "Cell"; run = cell }
    let sameTransactionExample = { name = "Same Transaction"; run = sameTransaction }
    let sendInCallbackExample = { name = "Send In Callback"; run = sendInCallback }
    let splitExample = { name = "Split"; run = split }
    let streamExample = { name = "Stream"; run = stream }
    let updatesExample = { name = "Updates"; run = updates }
    let value1Example = { name = "Value 1"; run = value1 }
    let value2Example = { name = "Value 2"; run = value2 }

    let examplesByChoice =
        [
            ('a', cellExample)
            ('b', sameTransactionExample)
            ('c', sendInCallbackExample)
            ('d', splitExample)
            ('e', streamExample)
            ('f', updatesExample)
            ('g', value1Example)
            ('h', value2Example)
        ] |> Map.ofSeq

    for choiceAndExample in examplesByChoice do printfn "(%c) %s" choiceAndExample.Key choiceAndExample.Value.name

    let rec getExampleToRun () =
        printfn ""
        printf "Select an example to run: "
        let c = Console.ReadKey()
        if Map.containsKey c.KeyChar examplesByChoice
        then Map.find c.KeyChar examplesByChoice
        else
            printfn ""
            printfn "Invalid selection."
            getExampleToRun ()

    let exampleToRun = getExampleToRun ()

    printfn ""
    printfn ""
    exampleToRun.run ()

    0