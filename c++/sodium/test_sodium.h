/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * All rights reserved.
 *
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _TEST_SODIUM_H_
#define _TEST_SODIUM_H_

#include <cppunit/TestCase.h>
#include <cppunit/extensions/HelperMacros.h>

class test_sodium : public CppUnit::TestFixture {
    CPPUNIT_TEST_SUITE(test_sodium);
    CPPUNIT_TEST(event1);
    CPPUNIT_TEST(map);
    CPPUNIT_TEST(merge_non_simultaneous);
    CPPUNIT_TEST(merge_simultaneous);
    CPPUNIT_TEST(coalesce);
    CPPUNIT_TEST_SUITE_END();

    void event1();
    void map();
    void merge_non_simultaneous();
    void merge_simultaneous();
    void coalesce();
};

#endif

