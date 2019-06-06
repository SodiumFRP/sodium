open System
open Sodium.Frp

type Example = { name : string; run : unit -> unit }

let mapL f (l : Lazy<_>) = lazy f l.Value

[<EntryPoint>]
let main _ =
    let calm () =
        let calmInternal init sA =
            let getState a lastA =
                let oa = Some a
                if oa = lastA then struct (None, lastA) else struct (oa, oa)
            sA |> collectLazyS init getState |> filterOptionS

        let calm a =
            let initA = a |> sampleLazyC
            let oInitA = initA |> mapL Some
            a |> updatesC |> calmInternal oInitA |> holdLazyS initA

        let sa = sinkC 1
        use _l = sa |> calm |> listenC (printfn "%i")
        sa |> sendC 1
        sa |> sendC 2
        sa |> sendC 2
        sa |> sendC 4
        sa |> sendC 4
        sa |> sendC 1

    let pause () =
        let pausableClock sPause sResume clock =
            let pauseTime = (sPause |> snapshotAndTakeC clock |> mapS Some, sResume |> mapToS None) |> orElseS |> holdS None
            let getLostTime (total : float) =
                let tPause = match pauseTime |> sampleC with | None -> 0.0 | Some v -> v
                let now = clock |> sampleC
                total + (now - tPause)
            let getLostTime' _ total = getLostTime total
            let lostTime = sResume |> accumS 0.0 getLostTime'
            let getClock pauseTime clock lostTime = (match pauseTime with | None -> clock | Some v -> v) - lostTime
            (pauseTime, clock, lostTime) |> lift3C getClock

        let mainClock = sinkC 0.0
        let sPause = sinkS ()
        let sResume = sinkS ()
        let gameClock = pausableClock sPause sResume mainClock

        let getOutput mainClock gameClock = sprintf "main=%f game=%f" mainClock gameClock
        use _l = (mainClock, gameClock) |> lift2C getOutput |> listenC (printfn "%s")
        mainClock |> sendC 1.0
        mainClock |> sendC 2.0
        mainClock |> sendC 3.0
        sPause |> sendS ()
        mainClock |> sendC 4.0
        mainClock |> sendC 5.0
        mainClock |> sendC 6.0
        sResume |> sendS ()
        mainClock |> sendC 7.0

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