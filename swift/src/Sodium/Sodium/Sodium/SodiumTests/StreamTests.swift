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
/*
    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
*/
    
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

    func testMap() {
        let e = StreamSink<Int>()
        let m = e.map{ $0.description }
        var out = [String]()
        let l = m.listen{ out.append($0) }
        e.send(5)
        l.unlisten()

        XCTAssert(["5"] == out, "testMap() failed \(out)")
    }

}
/*
public class TestStream extends TestCase {
 
 

    func testMapTo()
    {
        StreamSink<Integer> e = StreamSink()
        Stream<String> m = e.mapTo("fusebox")
        var out = [String]()
        Listener l = m.listen((String x) -> { out.add(x) })
        e.send(5)
        e.send(6)
        l.unlisten()
        assertEquals(Arrays.asList("fusebox", "fusebox"), out)
    }
    
    func testMergeNonSimultaneous()
    {
        StreamSink<Integer> e1 = StreamSink()
        StreamSink<Integer> e2 = StreamSink()
    var out = [Int]()
        Listener l = e2.orElse(e1).listen(x -> { out.add(x) })
        e1.send(7)
        e2.send(9)
        e1.send(8)
        l.unlisten()
        assertEquals(Arrays.asList(7,9,8), out)
    }
    
    func testMergeSimultaneous()
    {
        StreamSink<Integer> s1 = StreamSink((l,r) -> r)
        StreamSink<Integer> s2 = StreamSink((l,r) -> r)
    var out = [Int]()
        Listener l = s2.orElse(s1).listen(x -> { out.add(x) })
        Transaction.runVoid(() -> {
            s1.send(7)
            s2.send(60)
            })
        Transaction.runVoid(() -> {
            s1.send(9)
            })
        Transaction.runVoid(() -> {
            s1.send(7)
            s1.send(60)
            s2.send(8)
            s2.send(90)
            })
        Transaction.runVoid(() -> {
            s2.send(8)
            s2.send(90)
            s1.send(7)
            s1.send(60)
            })
        Transaction.runVoid(() -> {
            s2.send(8)
            s1.send(7)
            s2.send(90)
            s1.send(60)
            })
        l.unlisten()
        assertEquals(Arrays.asList(60,9,90,90,90), out)
    }
    
    func testCoalesce()
    {
        StreamSink<Integer> s = StreamSink<>((Integer a, Integer b) -> a+b)
    var out = [Int]()
        Listener l = s
            .listen((Integer x) -> { out.add(x) })
        Transaction.runVoid(() -> {
            s.send(2)
            })
        Transaction.runVoid(() -> {
            s.send(8)
            s.send(40)
            })
        l.unlisten()
        assertEquals(Arrays.asList(2, 48), out)
    }
    
    func testFilter()
    {
        StreamSink<Character> e = StreamSink()
        List<Character> out = ArrayList()
        Listener l = e.filter((Character c) -> Character.isUpperCase(c)).listen((Character c) -> { out.add(c) })
        e.send('H')
        e.send('o')
        e.send('I')
        l.unlisten()
        assertEquals(Arrays.asList('H','I'), out)
    }
    
    func testFilterOptional()
    {
        StreamSink<Optional<String>> e = StreamSink()
        var out = [String]()
        Listener l = Stream.filterOptional(e).listen(s -> { out.add(s) })
        e.send(Optional.of("tomato"))
        e.send(Optional.empty())
        e.send(Optional.of("peach"))
        l.unlisten()
        assertEquals(Arrays.asList("tomato","peach"), out)
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
        Listener l = ec.listen(x -> { out.add(x) })
        ea.send(2)
        ea.send(52)
        l.unlisten()
        assertEquals(Arrays.asList(2,7), out)
    }
    
    func testGate()
    {
        StreamSink<Character> ec = StreamSink()
        CellSink<Boolean> epred = CellSink(true)
        List<Character> out = ArrayList()
        Listener l = ec.gate(epred).listen(x -> { out.add(x) })
        ec.send('H')
        epred.send(false)
        ec.send('O')
        epred.send(true)
        ec.send('I')
        l.unlisten()
        assertEquals(Arrays.asList('H','I'), out)
    }
    
    func testCollect()
    {
        StreamSink<Integer> ea = StreamSink()
    var out = [Int]()
        Stream<Integer> sum = ea.collect(0,
                                         (a,s) -> Tuple2(a+s+100, a+s)
        )
        Listener l = sum.listen((x) -> { out.add(x) })
        ea.send(5)
        ea.send(7)
        ea.send(1)
        ea.send(2)
        ea.send(3)
        l.unlisten()
        assertEquals(Arrays.asList(105,112,113,115,118), out)
    }
    
    func testAccum()
    {
        StreamSink<Integer> ea = StreamSink()
    var out = [Int]()
        Cell<Integer> sum = ea.accum(100, (a,s)->a+s)
        Listener l = sum.listen((x) -> { out.add(x) })
        ea.send(5)
        ea.send(7)
        ea.send(1)
        ea.send(2)
        ea.send(3)
        l.unlisten()
        assertEquals(Arrays.asList(100,105,112,113,115,118), out)
    }
    
    func testOnce()
    {
        StreamSink<Character> e = StreamSink()
        List<Character> out = ArrayList()
        Listener l = e.once().listen((x) -> { out.add(x) })
        e.send('A')
        e.send('B')
        e.send('C')
        l.unlisten()
        assertEquals(Arrays.asList('A'), out)
    }
    
    func testDefer()
    {
        StreamSink<Character> e = StreamSink()
        Cell<Character> b = e.hold(' ')
        List<Character> out = ArrayList()
        Listener l = Operational.defer(e).snapshot(b).listen((x) -> { out.add(x) })
        e.send('C')
        e.send('B')
        e.send('A')
        l.unlisten()
        assertEquals(Arrays.asList('C','B','A'), out)
    }
}

*/