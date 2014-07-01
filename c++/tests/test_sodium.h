/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
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
#if !defined(NO_CXX11)
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
    CPPUNIT_TEST(once1);
    // behaviour tests
    CPPUNIT_TEST(collect2);
    CPPUNIT_TEST(hold1);
    CPPUNIT_TEST(snapshot1);
    CPPUNIT_TEST(value1);
    CPPUNIT_TEST(constant_behavior);
    CPPUNIT_TEST(value_then_map);
    CPPUNIT_TEST(value_twice_then_map);
    CPPUNIT_TEST(value_then_coalesce);
    CPPUNIT_TEST(value_twice_then_coalesce);
    CPPUNIT_TEST(value_then_snapshot);
    CPPUNIT_TEST(value_twice_then_snapshot);
    CPPUNIT_TEST(value_then_merge);
    CPPUNIT_TEST(value_then_filter);
    CPPUNIT_TEST(value_twice_then_filter);
    CPPUNIT_TEST(value_then_once);
    CPPUNIT_TEST(value_twice_then_once);
    CPPUNIT_TEST(value_late_listen);
    CPPUNIT_TEST(mapB1);
    CPPUNIT_TEST(mapB_late_listen);
    CPPUNIT_TEST(apply1);
    CPPUNIT_TEST(lift1);
    CPPUNIT_TEST(lift_glitch);
    CPPUNIT_TEST(hold_is_delayed);
    CPPUNIT_TEST(switch_b1);
    CPPUNIT_TEST(switch_e1);
    CPPUNIT_TEST(loop_behavior);
    CPPUNIT_TEST(split1);
    CPPUNIT_TEST(add_cleanup1);
    CPPUNIT_TEST(add_cleanup2);
#endif
    CPPUNIT_TEST_SUITE_END();

    void event1();
#if !defined(NO_CXX11)
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
    void once1();
    void collect2();
    void hold1();
    void snapshot1();
    void value1();
    void constant_behavior();
    void value_then_map();
    void value_twice_then_map();
    void value_then_coalesce();
    void value_twice_then_coalesce();
    void value_then_snapshot();
    void value_twice_then_snapshot();
    void value_then_merge();
    void value_then_filter();
    void value_twice_then_filter();
    void value_then_once();
    void value_twice_then_once();
    void value_late_listen();
    void mapB1();
    void mapB_late_listen();
    void apply1();
    void lift1();
    void lift_glitch();
    void hold_is_delayed();
    void switch_b1();
    void switch_e1();
    void loop_behavior();
    void split1();
    void add_cleanup1();
    void add_cleanup2();
#endif
};

#endif

