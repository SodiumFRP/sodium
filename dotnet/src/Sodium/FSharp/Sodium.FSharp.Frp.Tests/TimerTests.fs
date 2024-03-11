module Sodium.Frp.Tests.Timer

open System
open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp
open System.Threading
open Sodium.Frp.Time

[<TestFixture>]
type ``Timer Tests``() =

    [<Test>]
    member __.``Simultaneous Timer Events``() =
        let ts = SystemClockTimerSystem(fun e -> ()) :> ITimerSystem<DateTime>
        let time = ts.Time
        let l = List<DateTime>()
        Transaction.run(fun () ->
            let now = time |> Behavior.sample
            let a1 = ts.At(Cell.constant(Some(now.AddMilliseconds(99.0))))
            let a2 = ts.At(Cell.constant(Some(now.AddMilliseconds(100.0))))
            let a3 = ts.At(Cell.constant(Some(now.AddMilliseconds(100.0))))
            let m = Stream.orElseAll [a1;a2;a3]
            m |> Stream.listen l.Add |> ignore)
        Thread.Sleep 200
        Assert.That(l.Count, Is.EqualTo(2))