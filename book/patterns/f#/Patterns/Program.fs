open System
open Sodium

type Example = { name : string; run : unit -> unit }

[<EntryPoint>]
let main _ =
    let calm () =
        let calmInternal init sA =
            let getState a lastA =
                let oa = Option.Some a
                if oa = lastA then (Option.None, lastA) else (oa, oa)
            sA |> Stream.collectLazy getState init |> Stream.filterOption

        let calm a =
            let initA = a |> Cell.sampleLazy
            let oInitA = initA |> Lazy.map Option.Some
            a |> Operational.updates |> calmInternal oInitA |> Stream.holdLazy initA

        let sa = Cell.sink 1
        use _l = sa |> calm |> Cell.listen (printfn "%i")
        sa.Send 1
        sa.Send 2
        sa.Send 2
        sa.Send 4
        sa.Send 4
        sa.Send 1

    let pause () =
        let pausableClock sPause sResume clock =
            let pauseTime = sPause |> Stream.snapshotAndTakeCell clock |> Stream.map Option.Some |> Stream.orElse (sResume |> Stream.mapTo Option.None) |> Stream.hold Option.None
            let getLostTime (total : float) =
                let tPause = match pauseTime |> Cell.sample with | None -> 0.0 | Some v -> v
                let now = clock |> Cell.sample
                total + (now - tPause)
            let getLostTime' _ total = getLostTime total
            let lostTime = sResume |> Stream.accum getLostTime' 0.0
            let getClock pauseTime clock lostTime = (match pauseTime with | None -> clock | Some v -> v) - lostTime
            Cell.lift3 getClock pauseTime clock lostTime

        let mainClock = Cell.sink 0.0
        let sPause = Stream.sink ()
        let sResume = Stream.sink ()
        let gameClock = pausableClock sPause sResume mainClock

        let getOutput mainClock gameClock = sprintf "main=%f game=%f" mainClock gameClock
        use _l = Cell.lift2 getOutput mainClock gameClock |> Cell.listen (printfn "%s")
        mainClock.Send 1.0
        mainClock.Send 2.0
        mainClock.Send 3.0
        sPause.Send ()
        mainClock.Send 4.0
        mainClock.Send 5.0
        mainClock.Send 6.0
        sResume.Send ()
        mainClock.Send 7.0

    let calmExample = { name = "Calm"; run = calm }
    let pauseExample = { name = "Pause"; run = pause }

    let examplesByChoice =
        [
            ('a', calmExample)
            ('b', pauseExample)
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