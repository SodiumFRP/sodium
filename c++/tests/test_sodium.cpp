/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */

#include "test_sodium.h"
#include <sodium/sodium.h>
#include <boost/optional.hpp>

#include <cppunit/ui/text/TestRunner.h>
#include <stdio.h>
#include <ctype.h>
#include <iostream>

using namespace std;
using namespace sodium;
using namespace boost;


void test_sodium::tearDown()
{
#if defined(SODIUM_V2)
    sodium::collect_cycles();
#endif
}

void test_sodium::event1()
{
    event_sink<int> ev;
    std::shared_ptr<string> out = std::make_shared<string>();
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
    CPPUNIT_ASSERT_EQUAL(string("eo"), *out);
}

void test_sodium::map()
{
    event_sink<int> e;
    auto m = e.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    });
    std::shared_ptr<vector<string> > out = std::make_shared<vector<string> >();
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
    std::shared_ptr<vector<int> > out = std::make_shared<vector<int> >();
    auto unlisten = e1.merge(e2).listen([out] (const int& x) { out->push_back(x); });
    e1.send(7);
    e2.send(9);
    e1.send(8);
    unlisten();
    vector<int> shouldBe = {7,9,8};
    CPPUNIT_ASSERT(shouldBe == *out);
}

#if 0
void test_sodium::merge_left_bias()
{
    event_sink<string> e1;
    event_sink<string> e2;
    std::shared_ptr<vector<string> > out = std::make_shared<vector<string> >();
    event<string> e = e1.merge(e2);
    auto unlisten = e.listen([out] (const string& x) { out->push_back(x); });
    {
        transaction trans;
        e1.send("left1a");
        e1.send("left1b");
        e2.send("right1a");
        e2.send("right1b");
    }
    {
        transaction trans;
        e2.send("right2a");
        e2.send("right2b");
        e1.send("left2a");
        e1.send("left2b");
    }
    unlisten();
    vector<string> shouldBe = {
        string("left1a"), string("left1b"),
        string("right1a"), string("right1b"),
        string("left2a"), string("left2b"),
        string("right2a"), string("right2b") };
    CPPUNIT_ASSERT(shouldBe == *out);
}
#endif

#if 0
void test_sodium::merge_left_bias_2_common(   
    event_sink<string> e1,
    event_sink<string> e2,
    event_sink<string> e3,
    event<string> e,
    std::shared_ptr<vector<string> > out
)
{
    auto unlisten = e.listen([out] (const string& x) { out->push_back(x); });
    {
        transaction trans;
        e1.send("1a");
        e2.send("1b");
        e3.send("1c");
    }
    {
        transaction trans;
        e2.send("2b");
        e1.send("2a");
        e3.send("2c");
    }
    {
        transaction trans;
        e1.send("3a");
        e3.send("3c");
        e2.send("3b");
    }
    {
        transaction trans;
        e3.send("4c");
        e1.send("4a");
        e2.send("4b");
    }
    {
        transaction trans;
        e2.send("5b");
        e3.send("5c");
        e1.send("5a");
    }
    {
        transaction trans;
        e3.send("6c");
        e2.send("6b");
        e1.send("6a");
    }
    vector<string> shouldBe = {
        string("1a"), string("1b"), string("1c"),
        string("2a"), string("2b"), string("2c"),
        string("3a"), string("3b"), string("3c"),
        string("4a"), string("4b"), string("4c"),
        string("5a"), string("5b"), string("5c"),
        string("6a"), string("6b"), string("6c"),
    };
    CPPUNIT_ASSERT(shouldBe == *out);
}
#endif

#if 0
void test_sodium::merge_left_bias_2a()
{
    std::shared_ptr<vector<string> > out = std::make_shared<vector<string> >();
    event_sink<string> e1;
    event_sink<string> e2;
    event_sink<string> e3;
    event<string> e = e1.merge(e2.merge(e3));
    merge_left_bias_2_common(e1, e2, e3, e, out);
}
#endif

#if 0
void test_sodium::merge_left_bias_2b()
{
    std::shared_ptr<vector<string> > out = std::make_shared<vector<string> >();
    event_sink<string> e1;
    event_sink<string> e2;
    event_sink<string> e3;
    event<string> e = e1.merge(e2).merge(e3);
    merge_left_bias_2_common(e1, e2, e3, e, out);
}
#endif

#if 0
void test_sodium::merge_simultaneous()
{
    event_sink<int> e;
    auto out = std::make_shared<vector<int>>();
    auto unlisten = e.merge(e).listen([out] (const int& x) { out->push_back(x); });
    e.send(7);
    e.send(9);
    unlisten();
    vector<int> shouldBe = {7,7,9,9};
    CPPUNIT_ASSERT(shouldBe == *out);
}
#endif

#if 0
void test_sodium::coalesce()
{
    event_sink<int> e1;
    event_sink<int> e2;
    auto out = std::make_shared<vector<int>>();
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
#endif

void test_sodium::filter()
{
    event_sink<char> e;
    auto out = std::make_shared<string>();
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
    auto out = std::make_shared<vector<string>>();
    auto unlisten = filter_optional(e).listen([out] (const string& s) {
        out->push_back(s);
    });
    e.send(boost::optional<string>("tomato"));
    e.send(boost::optional<string>());
    e.send(boost::optional<string>("peach"));
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("tomato"), string("peach") }) == *out);
}

// Sodium v2 now fixes the memory leak in this code.  
void test_sodium::loop_event1()
{
    event_sink<int> ea;
    transaction trans;
    event_loop<int> eb;
    eb.loop(ea);
    trans.close();
    auto out = std::make_shared<vector<int>>();
    auto unlisten = eb.listen([out] (const int& x) { out->push_back(x); });
    ea.send(2);
    ea.send(52);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 2, 52 }) == *out);
}

// Sodium v2 now fixes the memory leak in this code.  
void test_sodium::loop_event2()
{
    event_sink<int> ea;
    event<int> ec;
    {
        transaction trans;
        event_loop<int> eb;
        ec = ea.map<int>([] (const int& x) { return x % 10; })
                    .merge(eb, [] (const int& x, const int& y) { return x+y; });
        auto eb_out = ea.map<int>([] (const int& x) { return x / 10; })
                        .filter([] (const int& x) { return x != 0; });
        eb.loop(eb_out);
    }
    auto out = std::make_shared<vector<int>>();
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
    auto out = std::make_shared<string>();
    auto unlisten = ec.gate(pred).listen([out] (const char& c) { *out += c; });
    ec.send('H');
    pred.send(false);
    ec.send('O');
    pred.send(true);
    ec.send('I');
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("HI"), *out);
}

void test_sodium::once1()
{
    event_sink<char> e;
    auto out = std::make_shared<string>();
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
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.updates().listen([out] (const int& x) { out->push_back(x); });
    e.send(2);
    e.send(9);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 2, 9 }) == *out);
}

void test_sodium::snapshot1()
{
    behavior_sink<int> b(0);
    b.send(2);  /* ### */
    /*
    event_sink<long> trigger;
    auto out = std::make_shared<vector<string>>();
    auto unlisten = trigger.snapshot<int,string>(b, [out] (const long& x, const int& y) -> string {
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
    */
}

void test_sodium::value1()
{
    behavior_sink<int> b(9);
    transaction trans;
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.value().listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

void test_sodium::value_const()
{
    behavior<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = b.value().listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9 }) == *out);
}

void test_sodium::constant_behavior()
{
    behavior_sink<int> b(12);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = b.value().listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 12 }) == *out);
}

void test_sodium::value_then_map()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = b.value().map<int>([] (const int& x) { return x + 100; })
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 109, 102, 107 }) == *out);
}

/*
 * This is used for tests where value() produces a single initial value on listen,
 * and then we double that up by causing that single initial event to be repeated.
 * This needs testing separately, because the code must be done carefully to achieve
 * this.
 */
template <class A>
event<A> doubleUp(const event<A>& ea)
{
    return ea.merge(ea);
}

#if 0
void test_sodium::value_twice_then_map()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = doubleUp<int>(b.value()).map<int>([] (const int& x) { return x + 100; })
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 109,109,102,102,107,107 }) == *out);
}
#endif

void test_sodium::value_then_coalesce()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = b.value().coalesce([] (const int& fst, const int& snd) -> int { return snd; })
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

#if 0
void test_sodium::value_twice_then_coalesce()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = doubleUp(b.value()).coalesce([] (const int& fst, const int& snd) -> int { return fst + snd; })
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 18, 4, 14 }) == *out);
}
#endif

void test_sodium::value_then_snapshot()
{
    behavior_sink<int> bi(9);
    behavior_sink<char> bc('a');
    auto out = std::make_shared<string>();
    transaction trans;
    auto unlisten = bi.value().snapshot(bc).listen([out] (const char& c) { *out += c; });
    trans.close();
    bc.send('b');
    bi.send(2);
    bc.send('c');
    bi.send(7);
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("abc"), *out);
}

#if 0
void test_sodium::value_twice_then_snapshot()
{
    behavior_sink<int> bi(9);
    behavior_sink<char> bc('a');
    auto out = std::make_shared<string>();
    transaction trans;
    auto unlisten = doubleUp(bi.value()).snapshot(bc).listen([out] (const char& c) { *out += c; });
    trans.close();
    bc.send('b');
    bi.send(2);
    bc.send('c');
    bi.send(7);
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("aabbcc"), *out);
}
#endif

void test_sodium::value_then_merge()
{
    behavior_sink<int> bi(9);
    behavior_sink<int> bj(2);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = bi.value().merge(bj.value(), [] (const int& x, const int& y) -> int { return x+y; })
        .listen([out] (const int& z) { out->push_back(z); });
    trans.close();
    bi.send(1);
    bj.send(4);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 11, 1, 4 }) == *out);
}

void test_sodium::value_then_filter1()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = b.value().filter([] (const int& x) { return true; })
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

void test_sodium::value_then_filter2a()
{
    behavior_sink<optional<int>> b = behavior_sink<optional<int>>(optional<int>(9));
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = filter_optional(b.value())
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(optional<int>());
    b.send(optional<int>(7));
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 7 }) == *out);
}

void test_sodium::value_then_filter2b()
{
    behavior_sink<optional<int>> b = behavior_sink<optional<int>>(optional<int>());
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = filter_optional(b.value())
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(optional<int>());
    b.send(optional<int>(7));
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 7 }) == *out);
}

#if 0
void test_sodium::value_twice_then_filter()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = doubleUp(b.value()).filter([] (const int& x) { return true; })
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 9, 2, 2, 7, 7 }) == *out);
}
#endif

void test_sodium::value_then_once()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = b.value().once()
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9 }) == *out);
}

#if 0
void test_sodium::value_twice_then_once()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = doubleUp(b.value()).once()
        .listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9 }) == *out);
}
#endif

void test_sodium::value_late_listen()
{
    behavior_sink<int> b(9);
    b.send(8);
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    auto unlisten = b.value().listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b.send(2);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 8, 2 }) == *out);
}

void test_sodium::value_then_switch()
{
    transaction trans;
    behavior_sink<int> b1(9);
    behavior_sink<int> b2(11);
    auto out = std::make_shared<vector<int>>();
    behavior_sink<event<int>> be(b1.value());
    auto unlisten = switch_e(be).listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    b1.send(10);
    // This is an odd sort of case. We want
    // 1. value() is supposed to simulate a behavior as an event, firing the current
    //   value once in the transaction when we listen to it.
    // 2. when we switch to a new event in switch_e(), any firing of the new event
    //   that happens in that transaction should be ignored, because behaviors are
    //   delayed. The switch should take place after the transaction.
    // So we might think that it's sensible for switch_e() to fire out the value of
    // the new behavior upon switching, in that same transaction. But this breaks
    // 2., so in this case we can't maintain the "behavior as an event" fiction.
    be.send(b2.value());  // This does NOT fire 11 for the reasons given above.
    b2.send(12);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 10, 12 }) == *out);
}

void test_sodium::mapB1()
{
    behavior_sink<int> b(6);
    auto out = std::make_shared<vector<string>>();
    transaction trans;
    auto unlisten = b.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    }).value().listen([out] (const string& x) { out->push_back(x); });
    trans.close();
    b.send(8);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("6"), string("8") }) == *out);
}

void test_sodium::mapB_late_listen()
{
    behavior_sink<int> b(6);
    auto out = std::make_shared<vector<string>>();
    b.send(2);
    transaction trans;
    auto unlisten = b.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    }).value().listen([out] (const string& x) { out->push_back(x); });
    trans.close();
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
    auto out = std::make_shared<vector<string>>();
    transaction trans;
    auto unlisten = apply<int, string>(bf, ba).value().listen([out] (const string& x) {
        out->push_back(x);
    });
    trans.close();
    bf.send([] (const int& b) { return string("12 ")+fmtInt(b); });
    ba.send(6);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("1 5"), string("12 5"), string("12 6") }) == *out);
}

void test_sodium::lift1()
{
    behavior_sink<int> a(1);
    behavior_sink<int> b(5);
    auto out = std::make_shared<vector<string>>();
    transaction trans;
    auto unlisten = lift<int,int,string>([] (const int& a, const int& b) {
        return fmtInt(a)+" "+fmtInt(b);
    }, a, b).value().listen([out] (const string& x) {
        out->push_back(x);
    });
    trans.close();
    a.send(12);
    b.send(6);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("1 5"), string("12 5"), string("12 6") }) == *out);
}

void test_sodium::lift_glitch()
{
    transaction trans;
    behavior_sink<int> a(1);
    behavior<int> a3 = a.map<int>([] (const int& x) { return x * 3; });
    behavior<int> a5 = a.map<int>([] (const int& x) { return x * 5; });
    behavior<string> b = lift<int,int,string>([] (const int& x, const int& y) {
        return fmtInt(x)+" "+fmtInt(y);
    }, a3, a5);
    auto out = std::make_shared<vector<string>>();
    auto unlisten = b.value().listen([out] (const string& s) { out->push_back(s); });
    trans.close();
    a.send(2);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("3 5"), string("6 10") }) == *out);
}

void test_sodium::hold_is_delayed()
{
    event_sink<int> e;
    behavior<int> h = e.hold(0);
    event<string> pair = e.snapshot<int,string>(h, [] (const int& a, const int& b) { return fmtInt(a) + " " + fmtInt(b); });
    auto out = std::make_shared<vector<string>>();
    auto unlisten = pair.listen([out] (const string& s) { out->push_back(s); });
    e.send(2);
    e.send(3);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({string("2 0"), string("3 2")}) == *out);
}

struct SB
{
    SB(optional<char> oa, optional<char> ob, optional<behavior<char>> osw) : oa(oa), ob(ob), osw(osw) {}
    optional<char> oa;
    optional<char> ob;
    optional<behavior<char>> osw;
};

void test_sodium::switch_b1()
{
    transaction trans;
    event_sink<SB> esb;
    // Split each field out of SB so we can update multiple behaviours in a
    // single transaction.
    behavior<char> ba = filter_optional(esb.map<optional<char>>([] (const SB& s) { return s.oa; })).hold('A');
    behavior<char> bb = filter_optional(esb.map<optional<char>>([] (const SB& s) { return s.ob; })).hold('a');
    behavior<behavior<char>> bsw = filter_optional(esb.map<optional<behavior<char>>>([] (const SB& s) { return s.osw; })).hold(ba);
    behavior<char> bo = switch_b(bsw);
    auto out = std::make_shared<string>();
    auto unlisten = bo.value().listen([out] (const char& c) { *out += c; });
    trans.close();
    esb.send(SB(optional<char>('B'),optional<char>('b'),optional<behavior<char>>()));
    esb.send(SB(optional<char>('C'),optional<char>('c'),optional<behavior<char>>(bb)));
    esb.send(SB(optional<char>('D'),optional<char>('d'),optional<behavior<char>>()));
    esb.send(SB(optional<char>('E'),optional<char>('e'),optional<behavior<char>>(ba)));
    esb.send(SB(optional<char>('F'),optional<char>('f'),optional<behavior<char>>()));
    esb.send(SB(optional<char>(),   optional<char>(),   optional<behavior<char>>(bb)));
    esb.send(SB(optional<char>(),   optional<char>(),   optional<behavior<char>>(ba)));
    esb.send(SB(optional<char>('G'),optional<char>('g'),optional<behavior<char>>(bb)));
    esb.send(SB(optional<char>('H'),optional<char>('h'),optional<behavior<char>>(ba)));
    esb.send(SB(optional<char>('I'),optional<char>('i'),optional<behavior<char>>(ba)));
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("ABcdEFfFgHI"), *out);
}

struct SE
{
    SE(optional<char> oa, optional<char> ob, optional<event<char>> osw) : oa(oa), ob(ob), osw(osw) {}
    optional<char> oa;
    optional<char> ob;
    optional<event<char>> osw;
};

void test_sodium::switch_e1()
{
    event_sink<SE> ese;
    event<char> ea = filter_optional(ese.map<optional<char>>([] (const SE& s) { return s.oa; }));
    event<char> eb = filter_optional(ese.map<optional<char>>([] (const SE& s) { return s.ob; }));
    behavior<event<char>> bsw = filter_optional(ese.map<optional<event<char>>>([] (const SE& s) { return s.osw; }))
        .hold(ea);
    event<char> eo = switch_e(bsw);
    auto out = std::make_shared<string>();
    auto unlisten = eo.listen([out] (const char& c) { *out += c; });
    ese.send(SE(optional<char>('A'),optional<char>('a'),optional<event<char>>()));
    ese.send(SE(optional<char>('B'),optional<char>('b'),optional<event<char>>()));
    ese.send(SE(optional<char>('C'),optional<char>('c'),optional<event<char>>(eb)));
    ese.send(SE(optional<char>('D'),optional<char>('d'),optional<event<char>>()));
    ese.send(SE(optional<char>('E'),optional<char>('e'),optional<event<char>>(ea)));
    ese.send(SE(optional<char>('F'),optional<char>('f'),optional<event<char>>()));
    ese.send(SE(optional<char>('G'),optional<char>('g'),optional<event<char>>(eb)));
    ese.send(SE(optional<char>('H'),optional<char>('h'),optional<event<char>>(ea)));
    ese.send(SE(optional<char>('I'),optional<char>('i'),optional<event<char>>(ea)));
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("ABCdeFGhI"), *out);
}

// NOTE! Currently this leaks memory.
void test_sodium::loop_behavior()
{
    event_sink<int> ea;
    transaction trans;
    behavior_loop<int> sum;
    sum.loop(ea.snapshot<int,int>(sum, [] (const int& x, const int& y) { return x+y; }).hold(0));
    auto out = std::make_shared<vector<int>>();
    auto unlisten = sum.value().listen([out] (const int& x) { out->push_back(x); });
    trans.close();

    ea.send(2);
    ea.send(3);
    ea.send(1);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 0, 2, 5, 6 }) == *out);
    CPPUNIT_ASSERT(sum.sample() == 6);
}

void test_sodium::collect1()
{
    event_sink<int> ea;
    auto out = std::make_shared<vector<int>>();
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
    CPPUNIT_ASSERT(vector<int>({ 105, 112, 113, 115, 118 }) == *out);
}

void test_sodium::collect2()
{
    event_sink<int> ea;
    auto out = std::make_shared<vector<int>>();
    transaction trans;
    behavior<int> sum = ea.hold(100).collect<int, int>(0, [] (const int& a, const int& s) {
        return tuple<int, int>(a+s, a+s);
    });
    auto unlisten = sum.value().listen([out] (const int& x) { out->push_back(x); });
    trans.close();
    ea.send(5);
    ea.send(7);
    ea.send(1);
    ea.send(2);
    ea.send(3);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 100, 105, 112, 113, 115, 118 }) == *out);
}

void test_sodium::accum1()
{
    event_sink<int> ea;
    auto out = std::make_shared<vector<int>>();
    behavior<int> sum = ea.accum<int>(100, [] (const int& a, const int& s) -> int {
        return a+s;
    });
    auto unlisten = sum.updates().listen([out] (const int& x) { out->push_back(x); });
    ea.send(5);
    ea.send(7);
    ea.send(1);
    ea.send(2);
    ea.send(3);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 105, 112, 113, 115, 118 }) == *out);
}

void test_sodium::split1()
{
    event_sink<string> ea;
    auto out = std::make_shared<vector<string>>();
    event<string> eo = split(ea.map<list<string>>([] (const string& text0) -> list<string> {
        size_t p;
        string text = text0;
        list<string> tokens;
        while ((p = text.find(' ')) != string::npos) {
            tokens.push_back(text.substr(0, p));
            text = text.substr(p+1);
        }
        if (text.length() != 0)
            tokens.push_back(text);
        return tokens;
    }))
    // coalesce so we'll fail if split didn't put each string into its own transaction
    .coalesce([] (const string& a, const string& b) { return b; });
    auto unlisten = eo.listen([out] (const string& x) { out->push_back(x); });
    ea.send("the common cormorant");
    ea.send("or shag");
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("the"), string("common"), string("cormorant"),
                                    string("or"), string("shag") }) == *out);
}

// TO DO: split2 from Haskell implementation

void test_sodium::add_cleanup1()
{
    auto out = std::make_shared<vector<string>>();
    {
        event_sink<string> ea;
        std::function<void()> unlisten;
        {
            event<string> eb = ea.map<string>([] (const string& x) { return x + "!"; })
                                 .add_cleanup([out] {out->push_back("<cleanup>");});
            unlisten = eb.listen([out] (const string& x) { out->push_back(x); });
            ea.send("custard apple");
        }
        ea.send("persimmon");
        unlisten();
        out->push_back("date");
    }
    /*
    for (auto it = out->begin(); it != out->end(); ++it)
        printf("%s\n", (*it).c_str());
        */
    CPPUNIT_ASSERT(vector<string>({ string("custard apple!"), string("persimmon!"), string("<cleanup>"),
                                    string("date") }) == *out);
}

void test_sodium::add_cleanup2()
{
    auto out = std::make_shared<vector<string>>();
    {
        event_sink<string> ea;
        std::function<void()> unlisten;
        {
            event<string> eb = ea.filter([] (const string& x) { return x != "ignore"; })
                                 .add_cleanup([out] {out->push_back("<cleanup>");});
            unlisten = eb.listen([out] (const string& x) { out->push_back(x); });
            ea.send("custard apple");
            ea.send("ignore");
        }
        ea.send("persimmon");
        unlisten();
        out->push_back("date");
    }
    /*
    for (auto it = out->begin(); it != out->end(); ++it)
        printf("%s\n", (*it).c_str());
        */
    CPPUNIT_ASSERT(vector<string>({ string("custard apple"), string("persimmon"), string("<cleanup>"),
                                    string("date") }) == *out);
}

void test_sodium::constant_value()
{
    auto out = std::make_shared<vector<string>>();
    transaction trans; 
    behavior<string> a("cheese");
    auto eValue = a.value();
    eValue.listen([out] (const string& x) { out->push_back(x); });
    trans.close();
    CPPUNIT_ASSERT(vector<string>({ string("cheese") }) == *out);
}

void test_sodium::loop_value()
{
    auto out = std::make_shared<vector<string>>();
    transaction trans; 
    behavior_loop<string> a;
    auto eValue = a.value();
    a.loop(behavior<string>("cheese"));
    eValue.listen([out] (const string& x) { out->push_back(x); });
    trans.close();
    CPPUNIT_ASSERT(vector<string>({ string("cheese") }) == *out);
}

void test_sodium::loop_value_snapshot()
{
    auto out = std::make_shared<vector<string>>();
    behavior<string> a("lettuce");
    transaction trans; 
    behavior_loop<string> b;
    auto eSnap = a.value().snapshot<string,string>(b, [] (const string& a, const string& b) {
        return a + " " + b;
    });
    b.loop(behavior<string>("cheese"));
    auto unlisten = eSnap.listen([out] (const string& x) { out->push_back(x); });
    trans.close();
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("lettuce cheese") }) == *out);
}

void test_sodium::loop_value_hold()
{
    auto out = std::make_shared<vector<string>>();
    transaction trans; 
    behavior_loop<string> a;
    behavior<string> value = a.value().hold("onion");
    event_sink<unit> eTick;
    a.loop(behavior<string>("cheese"));
    trans.close();
    auto unlisten = eTick.snapshot(value).listen([out] (const string& x) { out->push_back(x); });
    eTick.send(unit());
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("cheese") }) == *out);
}

void test_sodium::lift_loop()
{
    auto out = std::make_shared<vector<string>>();
    transaction trans;
    behavior_loop<string> a;
    behavior_sink<string> b("kettle");
    auto c = lift<string, string, string>([] (const string& a, const string& b) {
        return a+" "+b;
    }, a, b);
    a.loop(behavior<string>("tea"));
    auto unlisten = c.value().listen([out] (const string& x) { out->push_back(x); });
    trans.close();
    b.send("caddy");
    CPPUNIT_ASSERT(vector<string>({ string("tea kettle"), string("tea caddy") }) == *out);
}

void test_sodium::loop_switch_e()
{
    auto out = std::make_shared<vector<string>>();
    transaction trans;
    event_sink<string> e1;
    behavior_loop<event<string>> b_lp;
    event<string> e = switch_e(b_lp);
    e1.send("banana");
    auto unlisten = e.listen([out] (const string& x) { out->push_back(x); });
    behavior_sink<event<string>> b(e1);
    b_lp.loop(b);
    trans.close();
    event_sink<string> e2;
    e2.send("pear");
    b.send(e2);
    e2.send("apple");
    CPPUNIT_ASSERT(vector<string>({ string("banana"), string("apple") }) == *out);
}

void test_sodium::detach_sink()
{
    // Check that holding the sink doesn't prevent a cleanup added to an event
    // from working.
    event_sink<int>* esnk = new event_sink<int>;
    std::shared_ptr<bool> cleanedUp(new bool(false));
    event<int>* e(new event<int>(esnk->add_cleanup([cleanedUp] () {
        *cleanedUp = true;
    })));
    CPPUNIT_ASSERT(*cleanedUp == false);
    delete e;
    CPPUNIT_ASSERT(*cleanedUp == true);
    delete esnk;
}

void test_sodium::move_semantics()
{
    behavior<unique_ptr<int>> pointer(unique_ptr<int>(new int(625)));
    int v = 0;
    auto value = pointer.map<int>([&](const unique_ptr<int>& pInt) {
        return pInt ? *pInt : 0;
    });
    CPPUNIT_ASSERT(value.sample() == 625);
}

#if 0  // TO DO: Reinstate
void test_sodium::move_semantics_sink()
{
    behavior_sink<unique_ptr<int>> bs(unique_ptr<int>(new int(1)));

    int newValue = 0;
    bs.updates().listen([&](const unique_ptr<int>& pInt) {
        newValue = pInt ? *pInt : 0;
    });

    bs.send(unique_ptr<int>(new int(2)));
    CPPUNIT_ASSERT(newValue == 2);
}
#endif

void test_sodium::move_semantics_hold()
{
    event<unique_ptr<int>> e;
    auto b = e.hold(unique_ptr<int>(new int(345)));
    auto val = b.map<int>([](const unique_ptr<int>& pInt) {
        return pInt ? *pInt : 0;
    });
    CPPUNIT_ASSERT(val.sample() == 345);
}

void test_sodium::lift_from_simultaneous()
{
    transaction trans;
    behavior_sink<int> b1(3);
    behavior_sink<int> b2(5);
    behavior<int> sum = lift<int, int, int>(
        [] (const int& a, const int& b) { return a + b; }, b1, b2);
    auto out = std::make_shared<vector<int>>();
    auto kill = sum.value().listen([out] (const int& sum) {
        out->push_back(sum);
    });
    b2.send(7);
    trans.close();
    kill();
    CPPUNIT_ASSERT(vector<int>({ 10 }) == *out);
}

int main(int argc, char* argv[])
{
    for (int i = 0; i < 1; i++) {
        CppUnit::TextUi::TestRunner runner;
        runner.addTest( test_sodium::suite() );
        runner.run();
    }
    return 0;
}

