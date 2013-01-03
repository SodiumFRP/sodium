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
    CPPUNIT_TEST(filter);
    CPPUNIT_TEST(filter_optional1);
    CPPUNIT_TEST(loop_event);
    CPPUNIT_TEST(gate1);
    CPPUNIT_TEST(collect1);
    CPPUNIT_TEST(accum1);
    CPPUNIT_TEST(countE1);
    CPPUNIT_TEST(count1);
    //CPPUNIT_TEST(once1);
    CPPUNIT_TEST_SUITE_END();

    void event1();
    void map();
    void merge_non_simultaneous();
    void merge_simultaneous();
    void coalesce();
    void filter();
    void filter_optional1();
    void loop_event();
    void gate1();
    void collect1();
    void accum1();
    void countE1();
    void count1();
    //void once1();
};

#endif

