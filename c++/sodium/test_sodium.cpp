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

/*
    public void testCollect()
    {
        EventSink<Integer> ea = new EventSink();
        List<Integer> out = new ArrayList();
        Event<Integer> sum = ea.collect(100,
            //(a,s) -> new Tuple2(a+s, a+s)
            new Lambda2<Integer, Integer, Tuple2<Integer,Integer>>() {
                public Tuple2<Integer,Integer> apply(Integer a, Integer s) {
                    return new Tuple2<Integer,Integer>(a+s, a+s);
                }
            }
        );
        Listener l = sum.listen((x) -> { out.add(x); });
        ea.send(5);
        ea.send(7);
        ea.send(1);
        ea.send(2);
        ea.send(3);
        l.unlisten();
        assertEquals(Arrays.asList(105,112,113,115,118), out);
    }
    */
    
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
    CPPUNIT_ASSERT(std::vector<int>({ 1, 2, 3 }) == *out);
}

/*
void test_sodium::once1()
{
    event_sink<char> e;
    auto unlisten = e.once().listen([] (const char& x) { *out += c; });
    e.send('A');
    e.send('B');
    e.send('C');
    unlisten();
    CPPUNUIT_ASSERT_EQUALS(string("A"), *out);
}
*/

int main(int argc, char* argv[])
{
    CppUnit::TextUi::TestRunner runner;
    runner.addTest( test_sodium::suite() );
    runner.run();
    return 0;
}

