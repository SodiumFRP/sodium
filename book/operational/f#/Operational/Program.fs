open System
open Sodium.Frp

type Example = { name : string; run : unit -> unit }

[<EntryPoint>]
let main _ =
    let flip f x y = f y x

    let cell () =
        let x = sinkC 0
        use _l = x |> listenC (printfn "%i")
        x |> sendC 10
        x |> sendC 20
        x |> sendC 30

    let sameTransaction () =
        let sX = sinkS ()
        let sXPlus1 = sX |> mapS ((+) 1)
        use _l = runT (fun () ->
            sX |> sendS 1
            sXPlus1 |> listenS (printfn "%i"))
        sX |> sendS 2
        sX |> sendS 3

    let sendInCallback () =
        let sX = sinkS ()
        let sY = sinkS ()
        use _l =
            Listener.fromStrongList
                [
                    sX |> listenS (sY |> flip sendS)
                    sY |> listenS (printfn "%i")
                ]
        sX |> sendS 1
        sX |> sendS 2
        sX |> sendS 3

    let split () =
        let ``as`` = sinkS ()
        use _l = ``as`` |> Operational.split |> accumS 0 (+) |> updatesC |> listenS (printfn "%i")
        ``as`` |> sendS [ 100; 15; 60 ]
        ``as`` |> sendS [ 1; 5 ]

    let stream () =
        let sX = sinkS ()
        let sXPlus1 = sX |> mapS ((+) 1)
        use _l = sXPlus1 |> listenS (printfn "%i")
        sX |> sendS 1
        sX |> sendS 2
        sX |> sendS 3

    let updates () =
        let x = sinkC 0
        x |> sendC 1
        use _l = x |> updatesC |> listenS (printfn "%i")
        x |> sendC 2
        x |> sendC 3

    let value1 () =
        let x = sinkC 0
        x |> sendC 1
        use _l = x |> valuesC |> listenS (printfn "%i")
        x |> sendC 2
        x |> sendC 3

    let value2 () =
        let x = sinkC 0
        x |> sendC 1
        use _l = runT (fun () -> x |> valuesC |> listenS (printfn "%i"))
        x |> sendC 2
        x |> sendC 3

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