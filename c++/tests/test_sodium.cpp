/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */

#include "test_sodium.h"
#include <sodium/sodium.h>

#include <cppunit/ui/text/TestRunner.h>
#include <stdio.h>
#include <ctype.h>
#include <iostream>

using namespace std;
using namespace sodium;
using namespace boost;


#if defined(SODIUM_NO_CXX11)
struct append_char : i_lambda1<void, const int&> {
    append_char(const SODIUM_SHARED_PTR<string>& out) : out(out) {}
    SODIUM_SHARED_PTR<string> out;
    virtual void operator () (const int& ch) const {
        *out = *out + (char)ch;
    }
};
#endif

void test_sodium::event1()
{
#if defined(SODIUM_NO_CXX11)
    event_sink<int, def_part> ev;
    SODIUM_SHARED_PTR<string> out(new string);
#else
    event_sink<int> ev;
    auto out = std::make_shared<string>();
#endif
    ev.send('?');
#if defined(SODIUM_NO_CXX11)
    lambda0<void> unlisten;
#else
    function<void()> unlisten;
#endif
    {
        transaction<> trans;
        ev.send('h');
#if defined(SODIUM_NO_CXX11)
        unlisten = ev.listen(new append_char(out));
#else
        unlisten = ev.listen([out] (int ch) {
            *out = *out + (char)ch;
        });
#endif
        ev.send('e');
    };
    {
        transaction<> trans;
        ev.send('l');
        ev.send('l');
        ev.send('o');
    }
    unlisten();
    ev.send('!');
    CPPUNIT_ASSERT_EQUAL(string("hello"), *out);
}

#if !defined(SODIUM_NO_CXX11)
void test_sodium::map()
{
    event_sink<int> e;
    auto m = e.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    });
    auto out = std::make_shared<vector<string>>();
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
    auto out = std::make_shared<vector<int>>();
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
    auto out = std::make_shared<vector<int>>();
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

// NOTE! Currently this leaks memory.
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
}

void test_sodium::value1()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.value().listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

void test_sodium::constant_behavior()
{
    behavior_sink<int> b(12);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.value().listen([out] (const int& x) { out->push_back(x); });
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 12 }) == *out);
}

void test_sodium::value_then_map()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.value().map<int>([] (const int& x) { return x + 100; })
        .listen([out] (const int& x) { out->push_back(x); });
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

void test_sodium::value_twice_then_map()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = doubleUp<int>(b.value()).map<int>([] (const int& x) { return x + 100; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 109,109,102,102,107,107 }) == *out);
}

void test_sodium::value_then_coalesce()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.value().coalesce([] (const int& fst, const int& snd) -> int { return snd; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

void test_sodium::value_twice_then_coalesce()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = doubleUp(b.value()).coalesce([] (const int& fst, const int& snd) -> int { return fst + snd; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 18, 4, 14 }) == *out);
}

void test_sodium::value_then_snapshot()
{
    behavior_sink<int> bi(9);
    behavior_sink<char> bc('a');
    auto out = std::make_shared<string>();
    auto unlisten = bi.value().snapshot(bc).listen([out] (const char& c) { *out += c; });
    bc.send('b');
    bi.send(2);
    bc.send('c');
    bi.send(7);
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("abc"), *out);
}

void test_sodium::value_twice_then_snapshot()
{
    behavior_sink<int> bi(9);
    behavior_sink<char> bc('a');
    auto out = std::make_shared<string>();
    auto unlisten = doubleUp(bi.value()).snapshot(bc).listen([out] (const char& c) { *out += c; });
    bc.send('b');
    bi.send(2);
    bc.send('c');
    bi.send(7);
    unlisten();
    CPPUNIT_ASSERT_EQUAL(string("aabbcc"), *out);
}

void test_sodium::value_then_merge()
{
    behavior_sink<int> bi(9);
    behavior_sink<int> bj(2);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = bi.value().merge(bj.value(), [] (const int& x, const int& y) -> int { return x+y; })
        .listen([out] (const int& z) { out->push_back(z); });
    bi.send(1);
    bj.send(4);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 11, 1, 4 }) == *out);
}

void test_sodium::value_then_filter()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.value().filter([] (const int& x) { return true; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 2, 7 }) == *out);
}

void test_sodium::value_twice_then_filter()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = doubleUp(b.value()).filter([] (const int& x) { return true; })
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9, 9, 2, 2, 7, 7 }) == *out);
}

void test_sodium::value_then_once()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.value().once()
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9 }) == *out);
}

void test_sodium::value_twice_then_once()
{
    behavior_sink<int> b(9);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = doubleUp(b.value()).once()
        .listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    b.send(7);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 9 }) == *out);
}

void test_sodium::value_late_listen()
{
    behavior_sink<int> b(9);
    b.send(8);
    auto out = std::make_shared<vector<int>>();
    auto unlisten = b.value().listen([out] (const int& x) { out->push_back(x); });
    b.send(2);
    unlisten();
    CPPUNIT_ASSERT(vector<int>({ 8, 2 }) == *out);
}
	
void test_sodium::mapB1()
{
    behavior_sink<int> b(6);
    auto out = std::make_shared<vector<string>>();
    auto unlisten = b.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    }).value().listen([out] (const string& x) { out->push_back(x); });
    b.send(8);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("6"), string("8") }) == *out);
}

void test_sodium::mapB_late_listen()
{
    behavior_sink<int> b(6);
    auto out = std::make_shared<vector<string>>();
    b.send(2);
    auto unlisten = b.map<string>([] (const int& x) {
        char buf[128];
        sprintf(buf, "%d", x);
        return string(buf);
    }).value().listen([out] (const string& x) { out->push_back(x); });
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
    auto unlisten = apply<int, string>(bf, ba).value().listen([out] (const string& x) {
        out->push_back(x);
    });
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
    auto unlisten = lift<int,int,string>([] (const int& a, const int& b) {
        return fmtInt(a)+" "+fmtInt(b);
    }, a, b).value().listen([out] (const string& x) {
        out->push_back(x);
    });
    a.send(12);
    b.send(6);
    unlisten();
    CPPUNIT_ASSERT(vector<string>({ string("1 5"), string("12 5"), string("12 6") }) == *out);
}

void test_sodium::lift_glitch()
{
    behavior_sink<int> a(1);
    behavior<int> a3 = a.map<int>([] (const int& x) { return x * 3; });
    behavior<int> a5 = a.map<int>([] (const int& x) { return x * 5; });
    behavior<string> b = lift<int,int,string>([] (const int& x, const int& y) {
        return fmtInt(x)+" "+fmtInt(y);
    }, a3, a5);
    auto out = std::make_shared<vector<string>>();
    auto unlisten = b.value().listen([out] (const string& s) { out->push_back(s); });
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
    event_sink<SB> esb;
    // Split each field out of SB so we can update multiple behaviours in a
    // single transaction.
    behavior<char> ba = filter_optional(esb.map<optional<char>>([] (const SB& s) { return s.oa; })).hold('A');
    behavior<char> bb = filter_optional(esb.map<optional<char>>([] (const SB& s) { return s.ob; })).hold('a');
    behavior<behavior<char>> bsw = filter_optional(esb.map<optional<behavior<char>>>([] (const SB& s) { return s.osw; })).hold(ba);
    behavior<char> bo = switch_b(bsw);
    auto out = std::make_shared<string>();
    auto unlisten = bo.value().listen([out] (const char& c) { *out += c; });
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
    behavior_loop<int> sum;
    sum.loop(ea.snapshot<int,int>(sum, [] (const int& x, const int& y) { return x+y; }).hold(0));
    auto out = std::make_shared<vector<int>>();
    auto unlisten = sum.value().listen([out] (const int& x) { out->push_back(x); });
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
    behavior<int> sum = ea.hold(100).collect<int, int>(0, [] (const int& a, const int& s) {
        return tuple<int, int>(a+s, a+s);
    });
    auto unlisten = sum.value().listen([out] (const int& x) { out->push_back(x); });
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
#endif

int main(int argc, char* argv[])
{
    for (int i = 0; i < 100; i++) {
        CppUnit::TextUi::TestRunner runner;
        runner.addTest( test_sodium::suite() );
        runner.run();
    }
    return 0;
}

