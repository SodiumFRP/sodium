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
#include <iostream>

using namespace std;
using namespace sodium;


void test_sodium::event1()
{
    auto p = new_event<int>();
    auto ev = get<0>(p);
    auto push = get<1>(p);
    shared_ptr<string> pOut(new string);
    push('?');
    function<void()> unlisten;
    {
        transaction trans;
        push('h');
        unlisten = ev.listen([pOut] (int ch) {
            *pOut = *pOut + (char)ch;
        });
        push('e');
    };
    {
        transaction trans;
        push('l');
        push('l');
        push('o');
    }
    unlisten();
    push('!');
    CPPUNIT_ASSERT_EQUAL(string("hello"), *pOut);
}

void test_sodium::map()
{
    auto pe = new_event<int>();
    auto e = get<0>(pe);
    auto send_e = get<1>(pe);
    auto m = e.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    });
    shared_ptr<vector<string>> pOut(new vector<string>);
    auto unlisten = m.listen([pOut] (const string& x) { pOut->push_back(x); });
    send_e(5);
    unlisten();
    vector<string> shouldBe = { string("5") };
    CPPUNIT_ASSERT(shouldBe == *pOut);
}

void test_sodium::merge_non_simultaneous()
{
    auto pe1 = new_event<int>();
    auto e1 = get<0>(pe1);
    auto send_e1 = get<1>(pe1);
    auto pe2 = new_event<int>();
    auto e2 = get<0>(pe2);
    auto send_e2 = get<1>(pe2);
    shared_ptr<vector<int>> pOut(new vector<int>);
    auto unlisten = e1.merge(e2).listen([pOut] (const int& x) { pOut->push_back(x); });
    send_e1(7);
    send_e2(9);
    send_e1(8);
    unlisten();
    vector<int> shouldBe = {7,9,8};
    CPPUNIT_ASSERT(shouldBe == *pOut);
}

void test_sodium::merge_simultaneous()
{
    auto pe = new_event<int>();
    auto e = get<0>(pe);
    auto send_e = get<1>(pe);
    shared_ptr<vector<int>> pOut(new vector<int>);
    auto unlisten = e.merge(e).listen([pOut] (const int& x) { pOut->push_back(x); });
    send_e(7);
    send_e(9);
    unlisten();
    vector<int> shouldBe = {7,7,9,9};
    CPPUNIT_ASSERT(shouldBe == *pOut);
}

void test_sodium::coalesce()
{
    auto pe1 = new_event<int>();
    auto e1 = get<0>(pe1);
    auto send_e1 = get<1>(pe1);
    auto pe2 = new_event<int>();
    auto e2 = get<0>(pe2);
    auto send_e2 = get<1>(pe2);
    shared_ptr<vector<int>> pOut(new vector<int>);
    auto unlisten = e1.merge(e1.map<int>([] (const int& x) -> int { return x * 100; }).merge(e2))
                      .coalesce([] (const int& a, const int& b) -> int { return a+b; })
                      .listen([pOut] (const int& x) { pOut->push_back(x); });
    send_e1(2);
    send_e1(8);
    send_e2(40);
    unlisten();
    cerr << "items:" << endl;
    for (auto it = pOut->begin(); it != pOut->end(); ++it)
        cerr << *it << endl;
    vector<int> shouldBe = {202, 808, 40};
    CPPUNIT_ASSERT(shouldBe == *pOut);
}

int main(int argc, char* argv[])
{
    CppUnit::TextUi::TestRunner runner;
    runner.addTest( test_sodium::suite() );
    runner.run();
    return 0;
}

