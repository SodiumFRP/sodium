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
    // event tests
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
    CPPUNIT_TEST(once1);
    // behaviour tests
    CPPUNIT_TEST(hold1);
    CPPUNIT_TEST(snapshot1);
    CPPUNIT_TEST(values1);
    CPPUNIT_TEST(constant_behavior);
    CPPUNIT_TEST(values_then_map);
    CPPUNIT_TEST(values_twice_then_map);
    CPPUNIT_TEST(values_then_coalesce);
    CPPUNIT_TEST(values_twice_then_coalesce);
    CPPUNIT_TEST(values_then_snapshot);
    CPPUNIT_TEST(values_twice_then_snapshot);
    CPPUNIT_TEST(values_then_merge);
    CPPUNIT_TEST(values_then_filter);
    CPPUNIT_TEST(values_twice_then_filter);
    CPPUNIT_TEST(values_then_once);
    CPPUNIT_TEST(values_twice_then_once);
    CPPUNIT_TEST(values_late_listen);
    CPPUNIT_TEST(mapB1);
    CPPUNIT_TEST(mapB_late_listen);
    CPPUNIT_TEST(apply1);
    CPPUNIT_TEST(lift1);
    CPPUNIT_TEST(lift_glitch);
    CPPUNIT_TEST(hold_is_delayed);
    CPPUNIT_TEST(switch_b1);
    CPPUNIT_TEST(switch_e1);
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
    void once1();
    void hold1();
    void snapshot1();
    void values1();
    void constant_behavior();
    void values_then_map();
    void values_twice_then_map();
    void values_then_coalesce();
    void values_twice_then_coalesce();
    void values_then_snapshot();
    void values_twice_then_snapshot();
    void values_then_merge();
    void values_then_filter();
    void values_twice_then_filter();
    void values_then_once();
    void values_twice_then_once();
    void values_late_listen();
    void mapB1();
    void mapB_late_listen();
    void apply1();
    void lift1();
    void lift_glitch();
    void hold_is_delayed();
    void switch_b1();
    void switch_e1();
};

#endif

