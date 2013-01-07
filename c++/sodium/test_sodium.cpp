/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * All rights reserved.
 *
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */

#include "sodium/test_sodium.h"
#include "sodium/sodium.h"

#include <cppunit/ui/text/TestRunner.h>
#include <stdio.h>
#include <ctype.h>
#include <iostream>

using namespace std;
using namespace sodium;


void test_sodium::event1()
{
    event_sink<int> ev;
    shared_ptr<string> out(new string);
    ev.send('?');
    function<void()> unlisten;
    {
        transaction trans;
        ev.send('h');
        unlisten = ev.listen([out] (int ch) {
            *out = *out + (char)ch;
        });
        ev.send('e');
    };
    {
        transaction trans;
        ev.send('l');
        ev.send('l');
        ev.send('o');
    }
    unlisten();
    ev.send('!');
    CPPUNIT_ASSERT_EQUAL(string("hello"), *out);
}

void test_sodium::map()
{
    event_sink<int> e;
    auto m = e.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    });
    shared_ptr<vector<string>> out(new vector<string>);
    auto unlisten = m.listen([out] (const string& x) { out->push_back(x); });
    e.send(5);
    unlisten();
    vector<string> shouldBe = { string("5") };
    CPPUNIT_ASSERT(shouldBe == *out);
}

void test_sodium::merge_non_simultaneous()
{
    event_sink<int> e1;
    event_sink<int> e2;
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = e1.merge(e2).listen([out] (const int& x) { out->push_back(x); });
    e1.send(7);
    e2.send(9);
    e1.send(8);
    unlisten();
    vector<int> shouldBe = {7,9,8};
    CPPUNIT_ASSERT(shouldBe == *out);
}

void test_sodium::merge_simultaneous()
{
    event_sink<int> e;
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = e.merge(e).listen([out] (const int& x) { out->push_back(x); });
    e.send(7);
    e.send(9);
    unlisten();
    vector<int> shouldBe = {7,7,9,9};
    CPPUNIT_ASSERT(shouldBe == *out);
}

void test_sodium::coalesce()
{
    event_sink<int> e1;
    event_sink<int> e2;
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = e1.merge(e1.map<int>([] (const int& x) -> int { return x * 100; }).merge(e2))
                      .coalesce([] (const int& a, const int& b) -> int { return a+b; })
                      .listen([out] (const int& x) { out->push_back(x); });
    e1.send(2);
    e1.send(8);
    e2.send(40);
    unlisten();
    vector<int> shouldBe = {202, 808, 40};
    CPPUNIT_ASSERT(shouldBe == *out);
}

void test_sodium::filter()
{
    event_sink<char> e;
    shared_ptr<string> out(new string);
    auto unlisten = e.filter([] (const char& c) { return isupper(c); })
                     .listen([out] (const char& c) { (*out) += c; });
    e.send('H');
    e.send('o');
    e.send('I');
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("HI"), *out);
}

void test_sodium::filter_optional1()
{
    event_sink<boost::optional<string>> e;
    shared_ptr<vector<string>> out(new vector<string>);
    auto unlisten = filter_optional(e).listen([out] (const string& s) {
        out->push_back(s);
    });
    e.send(boost::optional<string>("tomato"));
    e.send(boost::optional<string>());
    e.send(boost::optional<string>("peach"));
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("tomato"), string("peach") }) == *out);
}

void test_sodium::loop_event()
{
    event_sink<int> ea;
    event<int> ec;
    {
        event_loop<int> eb;
        ec = ea.map<int>([] (const int& x) { return x % 10; })
                    .merge(eb, [] (const int& x, const int& y) { return x+y; });
        auto eb_out = ea.map<int>([] (const int& x) { return x / 10; })
                        .filter([] (const int& x) { return x != 0; });
        eb.loop(eb_out);
    }
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = ec.listen([out] (const int& x) { out->push_back(x); });
    ea.send(2);
    ea.send(52);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 2, 7 }) == *out);
}

void test_sodium::gate1()
{
    event_sink<char> ec;
    behavior_sink<bool> pred(true);
    shared_ptr<string> out(new string);
    auto unlisten = ec.gate(pred).listen([out] (const char& c) { *out += c; });
    ec.send('H');
    pred.send(false);
    ec.send('O');
    pred.send(true);
    ec.send('I');
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("HI"), *out);
}

void test_sodium::collect1()
{
    event_sink<int> ea;
    shared_ptr<vector<int>> out(new vector<int>);
    event<int> sum = ea.collect<int, int>(100, [] (const int& a, const int& s) {
        return tuple<int, int>(a+s, a+s);
    });
    auto unlisten = sum.listen([out] (const int& x) { out->push_back(x); });
    ea.send(5);
    ea.send(7);
    ea.send(1);
    ea.send(2);
    ea.send(3);
    unlisten();
    CPPUNIT_ASSERT(std::vector<int>({ 105, 112, 113, 115, 118 }) == *out);
}

void test_sodium::accum1()
{
    event_sink<int> ea;
    shared_ptr<vector<int>> out(new vector<int>);
    event<int> sum = ea.accum<int>(100, [] (const int& a, const int& s) -> int {
        return a+s;
    });
    auto unlisten = sum.listen([out] (const int& x) { out->push_back(x); });
    ea.send(5);
    ea.send(7);
    ea.send(1);
    ea.send(2);
    ea.send(3);
    unlisten();
    CPPUNIT_ASSERT(std::vector<int>({ 105, 112, 113, 115, 118 }) == *out);
}

void test_sodium::countE1()
{
    event_sink<unit> ea;
    shared_ptr<vector<int>> out(new vector<int>);
    event<int> sum = ea.countE();
    auto unlisten = sum.listen([out] (const int& x) { out->push_back(x); });
    ea.send(unit());
    ea.send(unit());
    ea.send(unit());
    unlisten();
    CPPUNIT_ASSERT(std::vector<int>({ 1, 2, 3 }) == *out);
}

void test_sodium::count1()
{
    event_sink<unit> ea;
    shared_ptr<vector<int>> out(new vector<int>);
    event<int> sum = ea.count().values();
    auto unlisten = sum.listen([out] (const int& x) { out->push_back(x); });
    ea.send(unit());
    ea.send(unit());
    ea.send(unit());
    unlisten();
    CPPUNIT_ASSERT(std::vector<int>({ 0, 1, 2, 3 }) == *out);
}

void test_sodium::once1()
{
    event_sink<char> e;
    shared_ptr<string> out(new string);
    auto unlisten = e.once().listen([out] (const char& c) { *out += c; });
    e.send('A');
    e.send('B');
    e.send('C');
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("A"), *out);
}

void test_sodium::hold1()
{
    event_sink<int> e;
    behavior<int> b = e.hold(0);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = b.changes().listen([out] (const int& x) { out->push_back(x); });
    e.send(2);
    e.send(9);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 2, 9 }) == *out);
}

void test_sodium::snapshot1()
{
    behavior_sink<int> b(0);
    event_sink<long> trigger;
    shared_ptr<vector<string>> out(new vector<string>);
    auto unlisten = trigger.snapshot<long,string>(b, [out] (const long& x, const int& y) -> string {
        char buf[129];
        sprintf(buf, "%ld %d", x, y);
        return buf;
    }).listen([out] (const string& s) {
        out->push_back(s);
    });
    trigger.send(100l);
    b.send(2);
    trigger.send(200l);
    b.send(9);
    b.send(1);
    trigger.send(300l);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("100 0"), string("200 2"), string("300 1") }) == *out);
}

void test_sodium::values1()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = b.values().listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

void test_sodium::constant_behavior()
{
    behavior_sink<int> b(12);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = b.values().listen([out] (const int& x) { out->push_back(x); });
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 12 }) == *out);
}

void test_sodium::values_then_map()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = b.values().map<int>([] (const int& x) { return x + 100; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 109, 102, 107 }) == *out);
}

/*
 * This is used for tests where values() produces a single initial value on listen,
 * and then we double that up by causing that single initial event to be repeated.
 * This needs testing separately, because the code must be done carefully to achieve
 * this.
 */
template <class A>
event<A> doubleUp(const event<A>& ea)
{
    return ea.merge(ea);
}

void test_sodium::values_twice_then_map()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = doubleUp<int>(b.values()).map<int>([] (const int& x) { return x + 100; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 109,109,102,102,107,107 }) == *out);
}

void test_sodium::values_then_coalesce()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = b.values().coalesce([] (const int& fst, const int& snd) -> int { return snd; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

void test_sodium::values_twice_then_coalesce()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = doubleUp(b.values()).coalesce([] (const int& fst, const int& snd) -> int { return fst + snd; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 18, 4, 14 }) == *out);
}

void test_sodium::values_then_snapshot()
{
    behavior_sink<int> bi(9);
    behavior_sink<char> bc('a');
    shared_ptr<string> out(new string);
    auto unlisten = bi.values().snapshot(bc).listen([out] (const char& c) { *out += c; });
    bc.send('b');
    bi.send(2);
    bc.send('c');
    bi.send(7);
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("abc"), *out);
}

void test_sodium::values_twice_then_snapshot()
{
    behavior_sink<int> bi(9);
    behavior_sink<char> bc('a');
    shared_ptr<string> out(new string);
    auto unlisten = doubleUp(bi.values()).snapshot(bc).listen([out] (const char& c) { *out += c; });
    bc.send('b');
    bi.send(2);
    bc.send('c');
    bi.send(7);
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("aabbcc"), *out);
}

void test_sodium::values_then_merge()
{
    behavior_sink<int> bi(9);
    behavior_sink<int> bj(2);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = bi.values().merge(bj.values(), [] (const int& x, const int& y) -> int { return x+y; })
        .listen([out] (const int& z) { out->push_back(z); });
    bi.send(1);
    bj.send(4);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 11, 1, 4 }) == *out);
}

void test_sodium::values_then_filter()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = b.values().filter([] (const int& x) { return true; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

void test_sodium::values_twice_then_filter()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = doubleUp(b.values()).filter([] (const int& x) { return true; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 9, 2, 2, 7, 7 }) == *out);
}

void test_sodium::values_then_once()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = b.values().once()
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9 }) == *out);
}

void test_sodium::values_twice_then_once()
{
    behavior_sink<int> b(9);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = doubleUp(b.values()).once()
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9 }) == *out);
}

void test_sodium::values_late_listen()
{
    behavior_sink<int> b(9);
    b.send(8);
    shared_ptr<vector<int>> out(new vector<int>);
    auto unlisten = b.values().listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 8, 2 }) == *out);
}
	
void test_sodium::mapB1()
{
    behavior_sink<int> b(6);
    shared_ptr<vector<string>> out(new vector<string>);
    auto unlisten = b.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    }).values().listen([out] (const string& x) { out->push_back(x); });
    b.send(8);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("6"), string("8") }) == *out);
}

void test_sodium::mapB_late_listen()
{
    behavior_sink<int> b(6);
    shared_ptr<vector<string>> out(new vector<string>);
    b.send(2);
    auto unlisten = b.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    }).values().listen([out] (const string& x) { out->push_back(x); });
    b.send(8);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("2"), string("8") }) == *out);
}

static string fmtInt(const int& x) {
    char buf[128];
    sprintf(buf, "%d", x);
    return string(buf);
}

void test_sodium::apply1()
{
    behavior_sink<function<string(const int&)>> bf([] (const int& b) {
        return string("1 ")+fmtInt(b);
    });
    behavior_sink<int> ba(5);
    shared_ptr<vector<string>> out(new vector<string>);
    auto unlisten = apply<int,string>(bf, ba).values().listen([out] (const string& x) {
        out->push_back(x);
    });
    bf.send([] (const int& b) { return string("12 ")+fmtInt(b); });
    ba.send(6);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("1 5"), string("12 5"), string("12 6") }) == *out);
}

int main(int argc, char* argv[])
{
    CppUnit::TextUi::TestRunner runner;
    runner.addTest( test_sodium::suite() );
    runner.run();
    return 0;
}

