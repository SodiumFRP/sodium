/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * All rights reserved.
 *
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */

#include "test_sodium.h"

using namespace sodium;


void test_sodium::event1()
{
    auto p = newEvent<int>();
    auto ev = get<0>(p);
    auto push = get<1>(p);
    std::shared_ptr<string> pOut(new string);
    push('?');
    std::function<void()> unlisten;
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

