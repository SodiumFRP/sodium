//
//  SodiumTests.swift
//  SodiumTests
//
//  Created by Andrew Bradnan on 4/27/16.
//  Copyright © 2016 Whirlygig Ventures. All rights reserved.
//

import XCTest
@testable import Sodium

class SodiumTests: XCTestCase {
    
    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
    
    func testPerformanceExample() {
        // This is an example of a performance test case.
        self.measureBlock {
            // Put the code you want to measure the time of here.
        }
    }
    
    func testCellSinkSend() {
        let c = CellSink<Int>(1234)
        
        c.send(2345)
        
        let v = c.sample()
        
        XCTAssert(2345 == v, "testCellSinkSend() failed \(v)")
    }
    
    func testValue() {
        var out = Array<Int>()
        do {
            let c = CellSink<Int>(9)
            let l = Transaction.noThrowRun { () -> Listener in
                //Operational.value(c).listen({ out.append($0) })
                let stream = Transaction.apply{ c.value($0) }
                let l2 = stream.listen{
                    out.append($0)
                }
                //stream.unlisten()
                return l2
            }
            defer { l.unlisten() }
            
            c.send(2)
            c.send(7)
        }
        XCTAssert([9,2,7] == out, "testValue failed \(out)")
    }

    func testMap() {
        let c = CellSink<Int>(6)
        var out = Array<String>()
        do {
            let l = c.map{ $0.description }.listen{ out.append($0) }
            defer { l.unlisten() }
            c.send(8)
        }
        XCTAssert(["6","8"] == out, "testMap() failed \(out)")
    }

    func testValueThenMap() {
        let c = CellSink<Int>(9)
        var out = Array<Int>()
        
        do {
            let l = Transaction.run{ Operational.value(c).map{ $0 + 100}.listen{ out.append($0) }}!
            defer { l.unlisten() }
            
            c.send(2)
            c.send(7)
        }
        XCTAssert([109,102,107] == out, "testValueThenMap() failed \(out)")
    }
    
    func testValueThenMerge()
    {
        let c1 = CellSink<Int>(9)
        let c2 = CellSink<Int>(2)
        var out = Array<Int>()
        do {
            let l = Transaction.run{
                Operational.value(c1).merge(Operational.value(c2), f: {(x, y) in x + y}).listen{ out.append($0) }}!
            defer { l.unlisten() }

            c1.send(1)
            c2.send(4)
        }
        XCTAssert([11,1,4] == out, "testValueThenMerge() failed \(out)")
    }


    func testValueThenOnce() {
        let c = CellSink<Int>(9)
        var out = Array<Int>()
        
        do {
            let l = Transaction.run { Operational.value(c).once().listen{ out.append($0) }}!
            defer { l.unlisten() }
            c.send(2)
            c.send(7)
        }
        XCTAssert([9] == out, "testValueThenOnce() failed \(out)")
    }

    func testValueThenFilter()
    {
        let c = CellSink<Int>(9)
        var out = Array<Int>()

        do {
            let l = Transaction.run{ Operational.value(c).filter{ $0 % 2 != 0}.listen{ out.append($0) }}!
            defer { l.unlisten() }
            
            c.send(2)
            c.send(7)
        }
        XCTAssert([9,7] == out, "testValueThenFilter() failed \(out)")
    }
    
    func testValueThenLateListen()
    {
        let c = CellSink<Int>(9)
        var out = Array<Int>()
        let value = Operational.value(c)
        c.send(8)
        do {
            let l = value.listen{ out.append($0) }
            defer { l.unlisten() }
            c.send(2)
            c.send(7)
        }
        XCTAssert([2,7] == out, "testValueThenLateListen() failed \(out)")
    }
    
    func testHoldLazy() {
        let c = CellSink<Int>(666)
        
        let foo = Transaction.apply{ (trans: Transaction) in
                    c.stream().holdLazy(trans, lazy: c.sampleLazy(trans)) }
        
        let v = foo.sample()
        
        XCTAssert(666 == v, "testHoldLazy() failed \(v)")
    }
    
    func testMapLateListen()
    {
        let c = CellSink<Int>(6)
        var out = [String]()
        let cm = c.map{ $0.description }
        c.send(2)
        do {
            let l = cm.listen{
                out.append($0)
            }
            defer { l.unlisten() }
            c.send(8)
        }
        XCTAssert(["2","8"] == out, "testMapLateListen() failed \(out)")
    }
    
    func testLift()
    {
        let c1 = CellSink<Int>(1)
        let c2 = CellSink<Int64>(5)
        var out = Array<String>()
        do {
            let l = c1.lift(c2, f: {(x: Int, y: Int64) in x.description + " " + y.description}).listen{ out.append($0) }
            defer { l.unlisten() }
            c1.send(12)
            c2.send(6)
        }
        XCTAssert(["1 5", "12 5", "12 6"] == out, "testList() failed \(out)")
    }

    func testLiftGlitch()
    {
        let c1 = CellSink<Int>(1)
        let c3 = c1.map{ $0 * 3 }
        let c5 = c1.map{ $0 * 5 }
        let c = c3.lift(c5, f: {(x, y) in x.description + " " + y.description})
        var out = Array<String>()
        do
        {
            let l = c.listen{ out.append($0) }
            defer { l.unlisten() }
            c1.send(2)
        }
        XCTAssert(["3 5", "6 10"] == out, "test() failed \(out)")
    }

    func testListen() {
        let c = CellSink<Int>(9)
        var out = [Int]()
        do {
            let l = c.listen { out.append($0) }
            defer { l.unlisten() }
            
            c.send(2)
            c.send(7)
        }
        XCTAssert([9,2,7] == out, "testListen failed")
    }

    func testListenOnce() {
        let c = CellSink<Int>(9)
        var out = [Int]()
        
        do {
            let l = Transaction.run{ Operational.value(c).listenOnce{ out.append($0) } }!!
            defer { l.unlisten() }

            c.send(2)
            c.send(7)
        }
        XCTAssert([9] == out, "testListenOnce() failed")
    }

    func testUpdates()
    {
        let c = CellSink<Int>(9)
        var out = Array<Int>()
        
        do {
            let l = Operational.updates(c).listen{ out.append($0) }
            defer { l.unlisten() }
            c.send(2)
            c.send(7)
        }
        XCTAssert([2,7] == out, "testUpdates() failed \(out)")
    }
    
    func testApply()
    {
        let cf = CellSink<Int64->String>({ (x:Int64) in "1 " + x.description})
        let ca = CellSink<Int64>(5)
        var out = Array<String>()
        
        do {
            let l = ca.apply(cf).listen{ out.append($0) }
            defer { l.unlisten() }
            cf.send({x in "12 " + x.description})
            ca.send(6)
        }
        XCTAssert(["1 5", "12 5", "12 6"] == out, "testApply() failed \(out)")
    }
    

    func testCellSink() {
        let x = CellSink<Int>(0)
        var out = [Int]()
        
        let l = x.listen{ out.append($0) }
        x.send(10)
        x.send(20)
        x.send(30)
        l.unlisten()
        XCTAssert([0,10,20,30] == out, "testCellSink() failed \(out)")
    }
    
    func testHold() {
        let s = StreamSink<Int>()
        let c = s.hold(0)
        var out = [Int]()
        do {
            let l = c.listen {
                out.append($0)
            }
            
            defer { l.unlisten() }
            
            //s.send(2)
            //s.send(9)
        }
        XCTAssert([0] == out, "testHold() failed \(out)")
    }

    func testListenTwice() {
        let s = StreamSink<String>()
        var out = [String]()
        
        let l1 = s.listen{ out.append($0) }
        let l2 = s.listen{ out.append($0) }
        
        s.send("one")
        s.send("two")
        
        l1.unlisten()
        l2.unlisten()
        
        s.unlisten()
        
        XCTAssert(["one", "one", "two", "two"] == out, "testListenTwice() failed \(out)")
    }
    func testHoldUpdates() {
        let s = StreamSink<Int>()
        let c = s.hold(0)
        var out = [Int]()
        do {
            //let s2 = Operational.updates(c)
            //let s3 = Transaction.apply(c.updates)
            //let l = s3.listen { out.append($0) }
            let l = s.listen { out.append($0) }
            defer { l.unlisten() }
            
            //Transaction.run{ trans in
            //    s3.send(trans, a: 2)
            //}
            //Transaction.run{ trans in
           //     s3.send(trans, a: 9)
            //}
//            s.send(2)
//            s.send(9)
        }
        XCTAssert([0] == out, "testHoldUpdates() failed \(out)")
    }

    func testLiftFromSimultaneous()
    {
        let t = Transaction.run{() -> (CellSink<Int>,CellSink<Int>) in
            let localC1 = CellSink<Int>(3)
            let localC2 = CellSink<Int>(5)
        
            localC2.send(7)
            return (localC1, localC2)
        }!
        
        let c1 = t.0
        let c2 = t.1
        var out = Array<Int>()
        do
        {
            let l = c1.lift(c2, f: {(x, y) in x + y}).listen{ out.append($0) }
            defer { l.unlisten() }
        }
        XCTAssert([10] == out, "testLiftFromSimultaneous() failed \(out)")
    }
    
    func testHoldIsDelayed()
    {
        let s = StreamSink<Int>()
        let h = s.hold(0)
        let pair = s.snapshot(h, f: {(a, b) in a.description + " " + b.description})
        var out = Array<String>()
        do
        {
            let l = pair.listen{ out.append($0) }
            defer { l.unlisten() }
            s.send(2)
            s.send(3)
        }
        XCTAssert(["2 0", "3 2"] == out, "testHoldIsDelayed() failed \(out)")
    }
    
    func testTransaction() {
        var calledBack = [Bool](arrayLiteral: false)
        
        Transaction.run{ trans in
            trans.prioritized(INode.Null, action: { trans2 in
                calledBack[0] = true
            }, dbg: "test")
        }

        XCTAssert(true == calledBack[0], "testTransaction() failed")
    }

    /*

    [Test]
    func testSnapshot()
    {
    let c = CellSink<Int>(0)
    StreamSink<long> trigger = StreamSink<long>()
    Array<string> @out = Array<string>()
    using (trigger.Snapshot(c, (x, y) => x + " " + y).Listen{ out.append($0) })
    {
    defer { l.Unlisten() }

    trigger.Send(100L)
    c.Send(2)
    trigger.Send(200L)
    c.Send(9)
    c.Send(1)
    trigger.Send(300L)
}
    XCTAssert([] == out, "test() failed \(out)")
CollectionAssert.AreEqual(new[] { "100 0", "200 2", "300 1" }, @out)
}

[Test]

[Test]
public async Task TestListenOnceTask()
{
    let c = CellSink<Int>(9)
    Int result = await Transaction.Run(() => Operational.Value(c).ListenOnce())
    c.Send(2)
    c.Send(7)
    Assert.AreEqual(9, result)
}

[Test]
[Test]
[Test]
[Test]
[Test]
[Test]
[Test]
[Test]
[Test]
[Test]
func testCalm()
{
    let c = CellSink<Int>(2)
    Array<Int> @out = Array<Int>()
    using (Transaction.Run(() => c.Calm().Listen{ out.append($0) }))
    {
    defer { l.Unlisten() }
        c.Send(2)
        c.Send(2)
        c.Send(4)
        c.Send(2)
        c.Send(4)
        c.Send(4)
        c.Send(2)
        c.Send(2)
    }
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(new[] { 2, 4, 2, 4, 2 }, @out)
}

[Test]
func testCalm2()
{
    let c = CellSink<Int>(2)
    Array<Int> @out = Array<Int>()
    using (Transaction.Run(() => c.Calm().Listen{ out.append($0) }))
    {
    defer { l.Unlisten() }
        c.Send(4)
        c.Send(2)
        c.Send(4)
        c.Send(4)
        c.Send(2)
        c.Send(2)
    }
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(new[] { 2, 4, 2, 4, 2 }, @out)
}

[Test]
[Test]
[Test]
[Test]
[Test]

private class Sc
{
    public readonly IMaybe<char> A
    public readonly IMaybe<char> B
    public readonly IMaybe<Cell<char>> Sw
    
    public Sc(IMaybe<char> a, IMaybe<char> b, IMaybe<Cell<char>> sw)
    {
    this.A = a
    this.B = b
    this.Sw = sw
    }
}

[Test]
func testSwitchC()
{
    StreamSink<Sc> ssc = StreamSink<Sc>()
    // Split each field out of SB so we can update multiple behaviors in a
    // single transaction.
    Cell<char> ca = ssc.Map(s => s.A).FilterMaybe().Hold('A')
    Cell<char> cb = ssc.Map(s => s.B).FilterMaybe().Hold('a')
    Cell<Cell<char>> csw = ssc.Map(s => s.Sw).FilterMaybe().Hold(ca)
    Cell<char> co = csw.SwitchC()
    Array<char> @out = Array<char>()
    using (co.Listen{ out.append($0) })
    {
    defer { l.Unlisten() }
        ssc.Send(Sc(Maybe.Just('B'), Maybe.Just('b'), Maybe.Nothing<Cell<char>>()))
        ssc.Send(Sc(Maybe.Just('C'), Maybe.Just('c'), Maybe.Just(cb)))
        ssc.Send(Sc(Maybe.Just('D'), Maybe.Just('d'), Maybe.Nothing<Cell<char>>()))
        ssc.Send(Sc(Maybe.Just('E'), Maybe.Just('e'), Maybe.Just(ca)))
        ssc.Send(Sc(Maybe.Just('F'), Maybe.Just('f'), Maybe.Nothing<Cell<char>>()))
        ssc.Send(Sc(Maybe.Nothing<char>(), Maybe.Nothing<char>(), Maybe.Just(cb)))
        ssc.Send(Sc(Maybe.Nothing<char>(), Maybe.Nothing<char>(), Maybe.Just(ca)))
        ssc.Send(Sc(Maybe.Just('G'), Maybe.Just('g'), Maybe.Just(cb)))
        ssc.Send(Sc(Maybe.Just('H'), Maybe.Just('h'), Maybe.Just(ca)))
        ssc.Send(Sc(Maybe.Just('I'), Maybe.Just('i'), Maybe.Just(ca)))
    }
    CollectionAssert.AreEqual(new[] { 'A', 'B', 'c', 'd', 'E', 'F', 'f', 'F', 'g', 'H', 'I' }, @out)
}

private class Sc2
{
    public readonly CellSink<Int> C
    
    public Sc2(Int initialValue)
    {
    this.C = CellSink<Int>(initialValue)
    }
}

[Test]
func testSwitchCSimultaneous()
{
    Sc2 sc1 = Sc2(0)
    CellSink<Sc2> csc = CellSink<Sc2>(sc1)
    Cell<Int> co = csc.Map<Cell<Int>>(b => b.C).SwitchC()
    Array<Int> @out = Array<Int>()
    using (co.Listen{ out.append($0) })
    {
    defer { l.Unlisten() }
        Sc2 sc2 = Sc2(3)
        Sc2 sc3 = Sc2(4)
        Sc2 sc4 = Sc2(7)
        sc1.C.Send(1)
        sc1.C.Send(2)
        csc.Send(sc2)
        sc1.C.Send(3)
        sc2.C.Send(4)
        sc3.C.Send(5)
        csc.Send(sc3)
        sc3.C.Send(6)
        sc3.C.Send(7)
        Transaction.RunVoid(() =>
            {
                sc3.C.Send(2)
                csc.Send(sc4)
                sc4.C.Send(8)
            })
        sc4.C.Send(9)
    }
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out)
}

private class Ss
{
    public readonly char A
    public readonly char B
    public readonly IMaybe<Stream<char>> Sw
    
    public Ss(char a, char b, IMaybe<Stream<char>> sw)
    {
    this.A = a
    this.B = b
    this.Sw = sw
    }
}

[Test]
func testSwitchS()
{
    StreamSink<Ss> sss = StreamSink<Ss>()
    // Split each field out of SB so we can update multiple behaviors in a
    // single transaction.
    Stream<char> sa = sss.Map(s => s.A)
    Stream<char> sb = sss.Map(s => s.B)
    Cell<Stream<char>> csw = sss.Map(s => s.Sw).FilterMaybe().Hold(sa)
    Stream<char> so = csw.SwitchS()
    Array<char> @out = Array<char>()
    using (so.Listen{ out.append($0) })
    {
    defer { l.Unlisten() }
        sss.Send(Ss('A', 'a', Maybe.Nothing<Stream<char>>()))
        sss.Send(Ss('B', 'b', Maybe.Nothing<Stream<char>>()))
        sss.Send(Ss('C', 'c', Maybe.Just(sb)))
        sss.Send(Ss('D', 'd', Maybe.Nothing<Stream<char>>()))
        sss.Send(Ss('E', 'e', Maybe.Just(sa)))
        sss.Send(Ss('F', 'f', Maybe.Nothing<Stream<char>>()))
        sss.Send(Ss('G', 'g', Maybe.Just(sb)))
        sss.Send(Ss('H', 'h', Maybe.Just(sa)))
        sss.Send(Ss('I', 'i', Maybe.Just(sa)))
    }
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(new[] { 'A', 'B', 'C', 'd', 'e', 'F', 'G', 'h', 'I' }, @out)
}

private class Ss2
{
    public readonly StreamSink<Int> S = StreamSink<Int>()
}

[Test]
func testSwitchSSimultaneous()
{
    Ss2 ss1 = Ss2()
    CellSink<Ss2> css = CellSink<Ss2>(ss1)
    Stream<Int> so = css.Map<Stream<Int>>(b => b.S).SwitchS()
    Array<Int> @out = Array<Int>()
    using (so.Listen{ out.append($0) })
    {
    defer { l.Unlisten() }
        Ss2 ss2 = Ss2()
        Ss2 ss3 = Ss2()
        Ss2 ss4 = Ss2()
        ss1.S.Send(0)
        ss1.S.Send(1)
        ss1.S.Send(2)
        css.Send(ss2)
        ss1.S.Send(7)
        ss2.S.Send(3)
        ss2.S.Send(4)
        ss3.S.Send(2)
        css.Send(ss3)
        ss3.S.Send(5)
        ss3.S.Send(6)
        ss3.S.Send(7)
        Transaction.RunVoid(() =>
            {
                ss3.S.Send(8)
                css.Send(ss4)
                ss4.S.Send(2)
            })
        ss4.S.Send(9)
    }
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out)
}

[Test]
func testLiftList()
{
    IReadOnlyList<CellSink<Int>> cellSinks = Enumerable.Range(0, 50).Select(_ => CellSink<Int>(1)).ToArray()
    Cell<Int> sum = cellSinks.Lift(v => v.Sum())
    Array<Int> @out = Array<Int>()
    using (sum.Listen{ out.append($0) })
    {
    defer { l.Unlisten() }
        cellSinks[4].Send(5)
        cellSinks[5].Send(5)
        Transaction.RunVoid(() =>
            {
                cellSinks[9].Send(5)
                cellSinks[17].Send(5)
                cellSinks[41].Send(5)
                cellSinks[48].Send(5)
            })
    }
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(new[] { 50, 54, 58, 74 }, @out)
}

[Test]
func testLiftListLarge()
{
    IReadOnlyList<CellSink<Int>> cellSinks = Enumerable.Range(0, 500).Select(_ => CellSink<Int>(1)).ToArray()
    Cell<Int> sum = cellSinks.Lift(v => v.Sum())
    Array<Int> @out = Array<Int>()
    using (sum.Listen{ out.append($0) })
    {
    defer { l.Unlisten() }
        cellSinks[4].Send(5)
        cellSinks[5].Send(5)
        Transaction.RunVoid(() =>
            {
                cellSinks[9].Send(5)
                cellSinks[17].Send(5)
                cellSinks[41].Send(5)
                cellSinks[48].Send(5)
            })
    }
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(new[] { 500, 504, 508, 524 }, @out)
}

[Test]
func testLiftListLargeManyUpdates()
{
    IReadOnlyList<CellSink<Int>> cellSinks = Enumerable.Range(0, 500).Select(_ => CellSink<Int>(1)).ToArray()
    Cell<Int> sum = cellSinks.Lift(v => v.Sum())
    Array<Int> @out = Array<Int>()
    using (sum.Listen{ out.append($0) })
    {
    defer { l.Unlisten() }
        for (Int i = 0 i < 100 i++)
        {
            Int n = i
            cellSinks[n * 5].Send(5)
            cellSinks[n * 5 + 1].Send(5)
            Transaction.RunVoid(() =>
                {
                    cellSinks[n * 5 + 2].Send(5)
                    cellSinks[n * 5 + 3].Send(5)
                    cellSinks[n * 5 + 4].Send(5)
                })
        }
    }
    IReadOnlyList<Int> expected = new[] { 500 }.Concat(Enumerable.Range(0, 100).SelectMany(n => new[] { 500 + 20 * n + 4, 500 + 20 * n + 8, 500 + 20 * n + 20 })).ToArray()
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(expected, @out)
}

[Test]
func testLiftListChangesWhileListening()
{
    IReadOnlyList<CellSink<Int>> cellSinks = Enumerable.Range(0, 50).Select(_ => CellSink<Int>(1)).ToArray()
    Cell<Int> sum = cellSinks.Lift(v => v.Sum())
    Array<Int> @out = Array<Int>()
    IListener l = Transaction.Run(() =>
    {
    cellSinks[4].Send(5)
    IListener lLocal = sum.Listen{ out.append($0) }
    cellSinks[5].Send(5)
    return lLocal
    })
    cellSinks[9].Send(5)
    Transaction.RunVoid(() =>
    {
    cellSinks[17].Send(5)
    cellSinks[41].Send(5)
    cellSinks[48].Send(5)
    })
    l.Unlisten()
    XCTAssert([] == out, "test() failed \(out)")
    CollectionAssert.AreEqual(new[] { 58, 62, 74 }, @out)
}

*/
}
