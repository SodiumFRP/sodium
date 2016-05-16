//
//  TestCommon.swift
//  Sodium
//
//  Created by Andrew Bradnan on 5/7/16.
//  Copyright © 2016 Whirlygig Ventures. All rights reserved.
//

import XCTest
@testable import Sodium

extension SodiumTests {

    func test_Base_send1() {
        let s = Transaction.run { StreamSink<String>() }!
        var out = [String]()
        let l = Transaction.run { s.listen{ out.append($0) } }!
        Transaction.runVoid{
            s.send("a")
        }
        Transaction.runVoid{
            s.send("b")
        }
        
        l.unlisten()

        XCTAssert(["a","b"] == out, "testSendStream() failed \(out)")
    }

    
    func test_Operational_split() {
        let a = Transaction.noThrowRun{ StreamSink<[String]>() }
        let b = Transaction.noThrowRun { _ in Operational.split(a) }
        var b_0 = [String]()
        let b_0_l = Transaction.noThrowRun { b.listen{ b_0.append($0) } }
        
        Transaction.runVoid{
            a.send(["a","b"])
        }
        b_0_l.unlisten()

        XCTAssert(["a","b"] == b_0, "testSplit() failed \(b_0)")
    }
    
    func test_Operational_defer1() {
        let a = Transaction.noThrowRun { StreamSink<String>() }
        let b = Transaction.noThrowRun { Operational.Defer(a) }

        var b_0 = [String]()
        let b_0_l = Transaction.noThrowRun{ b.listen{ b_0.append($0) } }
        
        Transaction.runVoid{
            a.send("a")
        }
        b_0_l.unlisten()
        XCTAssert(["a"] == b_0, "testDefer() failed \(b_0)")
        
        var b_1 = [String]()
        let b_1_l = Transaction.noThrowRun{ b.listen{ b_1.append($0) } }

        Transaction.runVoid{ a.send("b") }
        b_1_l.unlisten()
        XCTAssert(["b"] == b_1, "testDefer() failed \(b_1)")
    }
    
    func test_Operational_defer2() {
        let a = Transaction.run{ StreamSink<String>() }!
        let b = Transaction.run{ StreamSink<String>() }!
        let c = Transaction.run{ Operational.Defer(a).orElse(b) }!
    
        var c_0 = [String]()
        let c_0_l = Transaction.run{ c.listen{ c_0.append($0) } }!
    
        Transaction.runVoid{ a.send("a") }
        c_0_l.unlisten()
        XCTAssert(["a"] == c_0, "testDefer2() failed \(c_0)")

        var c_1 = [String]()
        let c_1_l = Transaction.run{ c.listen{ c_1.append($0) } }!
        Transaction.runVoid{
            a.send("b")
            b.send("B")
        }
        c_1_l.unlisten()
        XCTAssert(["B", "b"] == c_1, "testDefer2() failed \(c_1)")
    }

    func test_Stream_orElse1() {
        let a = Transaction.run{ StreamSink<Int>() }!
        let b = Transaction.run{ StreamSink<Int>() }!
        let c = Transaction.run{ a.orElse(b) }!
        var c_0 = [Int]()
        let c_0_l = Transaction.run{ c.listen{ c_0.append($0) } }!
    
        Transaction.runVoid{ a.send(0) }
        c_0_l.unlisten()
        XCTAssert([0] == c_0, "testOrElse1() failed \(c_0)")
    
        var c_1 = [Int]()
        let c_1_l = Transaction.run{ c.listen{ c_1.append($0) } }!
        Transaction.runVoid{ b.send(10) }
        c_1_l.unlisten()
        XCTAssert([10] == c_1, "testOrElse1() failed \(c_1)")
        var c_2 = [Int]()
        let c_2_l = Transaction.run{ c.listen{ c_2.append($0) } }!
        Transaction.runVoid{
            a.send(2)
            b.send(20)
        }
        c_2_l.unlisten()
        XCTAssert([2] == c_2, "testOrElse1() failed \(c_2)")
        
        var c_3 = [Int]()
        let c_3_l = Transaction.run{ c.listen{ c_3.append($0) } }!
        Transaction.runVoid{ b.send(30) }
        c_3_l.unlisten()
        XCTAssert([30] == c_3, "testOrElse1() failed \(c_3)")
    }

    func test_Operational_deferSimultaneous() {
        let a = Transaction.run{ StreamSink<String>() }!
        let b = Transaction.run{ StreamSink<String>() }!
        let c = Transaction.run{ Operational.Defer(a).orElse(Operational.Defer(b)) }!
        
        var c_0 = [String]()
        let c_0_l = Transaction.run{ c.listen{ c_0.append($0) } }!
        Transaction.runVoid{
            b.send("A")
        }
        c_0_l.unlisten()
        XCTAssert(["A"] == c_0, "testDeferSimultaneous() failed \(c_0)")

        var c_1 = [String]()
        let c_1_l = Transaction.run{ c.listen{ c_1.append($0) } }!
        Transaction.runVoid{
            a.send("b")
            b.send("B")
        }
        c_1_l.unlisten()
        XCTAssert(["b"] == c_1, "testDeferSimultaneous() failed \(c_1)")
    }
}