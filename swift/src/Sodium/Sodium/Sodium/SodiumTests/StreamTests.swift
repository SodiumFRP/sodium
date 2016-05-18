//
//  StreamTests.swift
//  Sodium
//
//  Created by Andrew Bradnan on 5/6/16.
//  Copyright © 2016 Whirlygig Ventures. All rights reserved.
//

import XCTest
@testable import Sodium

extension SodiumTests {
    
    func testSendStream() {
        let e = StreamSink<Int>()
        var out = [Int]()
        let l = e.listen{ out.append($0) }
        e.send(5)
        l.unlisten()
        XCTAssert([5] == out, "testSendStream() failed \(out)")
        e.send(6)
        XCTAssert([5] == out, "testSendStream() failed \(out)")
    }

    func testMapTo()
    {
        let e = StreamSink<Int>()
        let m = e.mapTo("fusebox")
        var out = [String]()
        let l = m.listen{ out.append($0) }
        e.send(5)
        e.send(6)
        l.unlisten()
        XCTAssert(["fusebox", "fusebox"] == out, "testMapTo() failed \(out)")
    }

    func workstestMergeNonSimultaneous()
    {
        let e1 = StreamSink<Int>()
        let e2 = StreamSink<Int>()
        var out = [Int]()
        let l = e2.orElse(e1).listen{ out.append($0) }
        e1.send(7)
        e2.send(9)
        e1.send(8)
        l.unlisten()
        XCTAssert([7,9,8] == out, "testMergeNonSimultaneous() failed \(out)")
    }

    func workstestMergeSimultaneous()
    {
        let s1 = StreamSink<Int>(fold: { (l,r) in r })
        let s2 = StreamSink<Int>(fold: { (l,r) in r })
        var out = [Int]()
        let l = s2.orElse(s1).listen{ out.append($0) }
        Transaction.runVoid{
            s1.send(7)
            s2.send(60)
            }
        Transaction.runVoid{
            s1.send(9)
            }
        Transaction.runVoid{
            s1.send(7)
            s1.send(60)
            s2.send(8)
            s2.send(90)
            }
        Transaction.runVoid{
            s2.send(8)
            s2.send(90)
            s1.send(7)
            s1.send(60)
            }
        Transaction.runVoid{
            s2.send(8)
            s1.send(7)
            s2.send(90)
            s1.send(60)
            }
        l.unlisten()
        XCTAssert([60,9,90,90,90] == out, "testMergeSimultaneous() failed \(out)")
    }

    func testCoalesce()
    {
        let s = StreamSink<Int>(fold: { $0 + $1 })
        var out = [Int]()
        let l = s.listen{ out.append($0) }
        Transaction.runVoid{
            s.send(2)
        }
        Transaction.runVoid{
            s.send(8)
            s.send(40)
        }
        l.unlisten()
        XCTAssert([2,48] == out, "testCoalesces() failed \(out)")
    }

    func testFilter()
    {
        let e = StreamSink<String>()
        var out = [String]()
        let l = e.filter{ $0.isUpperCase}.listen{ out.append($0) }
        e.send("H")
        e.send("o")
        e.send("I")
        l.unlisten()
        XCTAssert(["H","I"] == out, "testFilter() failed \(out)")
    }
    
    func testAccum()
    {
        let ea = StreamSink<Int>()
        var out = [Int]()
        let sum = ea.accum(100, f: {(a,s) in
            a+s
        })
        let l = sum.listen{ out.append($0) }
        ea.send(5)
        ea.send(7)
        ea.send(1)
        ea.send(2)
        ea.send(3)
        l.unlisten()
        XCTAssert([100,105,112,113,115,118] == out, "testAccum() failed \(out)")
    }

    func testGate()
    {
        let ec = StreamSink<String>()
        let epred = CellSink(true)
        var out = [String]()
        let l = ec.gate(epred).listen{ out.append($0!) }
        ec.send("H")
        epred.send(false)
        ec.send("O")
        epred.send(true)
        ec.send("I")
        l.unlisten()
        XCTAssert(["H","I"] == out, "testGate() failed \(out)")
    }
    
    func testCollect()
    {
        let ea = StreamSink<Int>()
        var out = [Int]()
        let sum = ea.collect(0,
            f: {(a,s) in (a+s+100, a+s) })
        
        let l = sum.listen{ out.append($0) }
        ea.send(5)
        ea.send(7)
        ea.send(1)
        ea.send(2)
        ea.send(3)
        l.unlisten()
        XCTAssert([105,112,113,115,118] == out, "testCollect() failed \(out)")
    }
    
    func testOnce()
    {
        let e = StreamSink<String>()
        var out = [String]()
        let l = e.once().listen{ out.append($0) }
        e.send("A")
        e.send("B")
        e.send("C")
        l.unlisten()
        XCTAssert(["A"] == out, "testOnce() failed \(out)")
    }
    
    func testConstantBehavior() {
        let b = Cell<Int>(value: 12)
        var out = [Int]()
        let l = b.listen{
            out.append($0)
        }
        l.unlisten()
        XCTAssert([12] == out, "testConstantBehavior() failed \(out)")
    }

/*
    func testSwitchAndDefer() {
        var out = [String]()
        let si = StreamSink<Int>()
        let l = Cell.switchS(si.map { Operational.Defer(Operational.value(Cell("A" + $0))) }
            .hold(Stream<String>())).listen{ out.append($0) }
        si.send(2)
        si.send(4)
        l.unlisten()
        XCTAssert(["A2","A4"] == out, "testSwitchAndDefer() failed \(out)")
    }
*/
    
}
/*
public class TestStream extends TestCase {
    func testDefer()
    {
    let e = StreamSink<Character>()
    let b = e.hold(" ")
    var out = [Character]()
    let l = Operational.Defer(e).snapshot(b).listen{ out.append($0) }
    e.send("C")
    e.send("B")
    e.send("A")
    l.unlisten()
    XCTAssert(["C","B","A"] == out, "testDefer() failed \(out)")
    }

    func testLoopStream()
    {
        final StreamSink<Integer> ea = StreamSink()
        Stream<Integer> ec = Transaction.<Stream<Integer>>run(() -> {
            StreamLoop<Integer> eb = StreamLoop<Integer>()
            Stream<Integer> ec_ = ea.map(x -> x % 10).merge(eb, (x, y) -> x+y)
            Stream<Integer> eb_out = ea.map(x -> x / 10).filter(x -> x != 0)
            eb.loop(eb_out)
            return ec_
            })
    var out = [Int]()
        let l = ec.listen(x -> { out.add(x) })
        ea.send(2)
        ea.send(52)
        l.unlisten()

        XCTAssert(["H","I"] == out, "testFilter() failed \(out)")
        assertEquals(Arrays.asList(2,7), out)
    }
    


func testFilterOptional()
{
let e = StreamSink<String?>()
var out = [String]()
let l = Stream.filterOptional(e).listen{ out.append($0) }
e.send("tomato")
e.send(nil)
e.send("peach")
l.unlisten()
XCTAssert(["tomato","peach"] == out, "testFilterOptional() failed \(out)")
}

}

*/