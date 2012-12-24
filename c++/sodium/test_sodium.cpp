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
    shared_ptr<string> pOut(new string);
    ev.send('?');
    function<void()> unlisten;
    {
        transaction trans;
        ev.send('h');
        unlisten = ev.listen([pOut] (int ch) {
            *pOut = *pOut + (char)ch;
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
    CPPUNIT_ASSERT_EQUAL(string("hello"), *pOut);
}

void test_sodium::map()
{
    event_sink<int> e;
    auto m = e.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    });
    shared_ptr<vector<string>> pOut(new vector<string>);
    auto unlisten = m.listen([pOut] (const string& x) { pOut->push_back(x); });
    e.send(5);
    unlisten();
    vector<string> shouldBe = { string("5") };
    CPPUNIT_ASSERT(shouldBe == *pOut);
}

void test_sodium::merge_non_simultaneous()
{
    event_sink<int> e1;
    event_sink<int> e2;
    shared_ptr<vector<int>> pOut(new vector<int>);
    auto unlisten = e1.merge(e2).listen([pOut] (const int& x) { pOut->push_back(x); });
    e1.send(7);
    e2.send(9);
    e1.send(8);
    unlisten();
    vector<int> shouldBe = {7,9,8};
    CPPUNIT_ASSERT(shouldBe == *pOut);
}

void test_sodium::merge_simultaneous()
{
    event_sink<int> e;
    shared_ptr<vector<int>> pOut(new vector<int>);
    auto unlisten = e.merge(e).listen([pOut] (const int& x) { pOut->push_back(x); });
    e.send(7);
    e.send(9);
    unlisten();
    vector<int> shouldBe = {7,7,9,9};
    CPPUNIT_ASSERT(shouldBe == *pOut);
}

void test_sodium::coalesce()
{
    event_sink<int> e1;
    event_sink<int> e2;
    shared_ptr<vector<int>> pOut(new vector<int>);
    auto unlisten = e1.merge(e1.map<int>([] (const int& x) -> int { return x * 100; }).merge(e2))
                      .coalesce([] (const int& a, const int& b) -> int { return a+b; })
                      .listen([pOut] (const int& x) { pOut->push_back(x); });
    e1.send(2);
    e1.send(8);
    e2.send(40);
    unlisten();
    vector<int> shouldBe = {202, 808, 40};
    CPPUNIT_ASSERT(shouldBe == *pOut);
}

void test_sodium::filter()
{
    event_sink<char> e;
    shared_ptr<string> pOut(new string);
    auto unlisten = e.filter([] (const char& c) { return isupper(c); })
                     .listen([pOut] (const char& c) { (*pOut) += c; });
    e.send('H');
    e.send('o');
    e.send('I');
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("HI"), *pOut);
}

void test_sodium::filter_optional1()
{
    event_sink<boost::optional<string>> e;
    shared_ptr<vector<string>> pOut(new vector<string>);
    auto unlisten = filter_optional(e).listen([pOut] (const string& s) {
        pOut->push_back(s);
    });
    e.send(boost::optional<string>("tomato"));
    e.send(boost::optional<string>());
    e.send(boost::optional<string>("peach"));
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("tomato"), string("peach") }) == *pOut);
}

#if 0
void test_sodium::loop_event()
{
    auto p = new_event<int>();
    auto ea = get<0>(p);
    auto send_ea = get<1>(p);
    event<int> ec;
    {
        auto lp = event_loop<int>();
        auto eb = get<0>(lp);
        auto loop = get<1>(lp);
        ec = ea.map<int>([] (const int& x) { return x % 10; })
                    .merge(eb, [] (const int& x, const int& y) { return x+y; });
        auto eb_out = ea.map<int>([] (const int& x) { return x / 10; })
                        .filter([] (const int& x) { return x != 0; });
        loop(eb_out);
    }
    shared_ptr<vector<int>> pOut(new vector<int>);
    auto unlisten = ec.listen([pOut] (const int& x) { pOut->push_back(x); });
    send_ea(2);
    send_ea(52);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 2, 7 }) == *pOut);
}
#endif

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
    shared_ptr<vector<int>> pOut(new vector<int>);
    auto unlisten = ec.listen([pOut] (const int& x) { pOut->push_back(x); });
    ea.send(2);
    ea.send(52);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 2, 7 }) == *pOut);
}

void test_sodium::gate1()
{
    event_sink<char> ec;
    behavior_sink<bool> pred(true);
    shared_ptr<string> pOut(new string);
    auto unlisten = ec.gate(pred).listen([pOut] (const char& c) { *pOut += c; });
    ec.send('H');
    pred.send(false);
    ec.send('O');
    pred.send(true);
    ec.send('I');
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("HI"), *pOut);
}

int main(int argc, char* argv[])
{
    CppUnit::TextUi::TestRunner runner;
    runner.addTest( test_sodium::suite() );
    runner.run();
    return 0;
}

