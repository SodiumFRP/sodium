/*
 * main.c++
 *
 *  Created on: 9/04/2012
 *      Author: Anthony Jones <ajones@clear.net.nz>
 */

#include <assert.h>
#include <stdio.h>
#include "lithium.h"

class point {
public:
    point(int x, int y) :
            x(x), y(y) {
    }
    int x;
    int y;
};

static auto transpose(std::shared_ptr<point> p) -> std::shared_ptr<point> {
    return std::shared_ptr < point > (new point(p->y, p->x));
}

static void test_map_e() {
    printf("%s\n", __FUNCTION__);
    auto r = li::receiver_e<point>();
    auto m = li::map_e<point, point>(transpose, r);
    std::shared_ptr<point> res;
    auto io = li::map_io<point>([&](std::shared_ptr<point> p) {
        res = p;
    }, m);
    li::send_event(std::shared_ptr < point > (new point(3, 5)), r);
    assert(res->x == 5);
    assert(res->y == 3);
}

static void test_merge_e() {
    printf("%s\n", __FUNCTION__);
    auto r0 = li::receiver_e<point>();
    auto r1 = li::receiver_e<point>();
    auto m = li::merge_e<point>(r0, r1);
    std::shared_ptr<point> res;
    auto io = li::map_io<point>([&](std::shared_ptr<point> p) {
        res = p;
    }, m);
    li::send_event(std::shared_ptr < point > (new point(3, 5)), r0);
    assert(res->x == 3);
    assert(res->y == 5);
    li::send_event(std::shared_ptr < point > (new point(9, 1)), r1);
    assert(res->x == 9);
    assert(res->y == 1);
}

static void test_filter_e() {
    printf("%s\n", __FUNCTION__);
    auto r = li::receiver_e<point>();
    auto m = li::filter_e<point>(r, [=](std::shared_ptr<point> p)->bool {
        return p->x < p->y;
    });
    std::shared_ptr<point> res;
    auto io = li::map_io<point>([&](std::shared_ptr<point> p) {
        res = p;
    }, m);
    li::send_event(std::shared_ptr < point > (new point(3, 5)), r);
    assert(res->x == 3);
    assert(res->y == 5);
    li::send_event(std::shared_ptr < point > (new point(9, 1)), r);
    assert(res->x == 3);
    assert(res->y == 5);
}

static void test_not_e() {
    printf("%s\n", __FUNCTION__);
    auto r = li::receiver_e<bool>();
    auto m = li::not_e(r);
    std::shared_ptr<bool> res;
    auto io = li::map_io<bool>([&](std::shared_ptr<bool> p) {
        res = p;
    }, m);
    li::send_event(std::shared_ptr <bool> (new bool(true)), r);
    assert(!*res);
    li::send_event(std::shared_ptr <bool> (new bool(false)), r);
    assert(*res);
}

int main(int argc, char *argv[]) {
    test_map_e();
    test_merge_e();
    test_filter_e();
    test_not_e();

    printf("tests passed\n");
}

