/*
 * main.c++
 *
 *  Created on: 9/04/2012
 *      Author: Anthony Jones <ajones@clear.net.nz>
 */

#include <assert.h>
#include <stdio.h>
#include "sodium.h"

class point {
public:
	point() {
	}
	point(int x, int y) :
			x(x), y(y) {
	}
	int x;
	int y;
};

static auto addxy(point p) -> int {
	return p.y + p.x;
}

static void test_map_e() {
	printf("%s\n", __FUNCTION__);
	auto r = na::receiver_e<point>();
	auto m = na::map_e<point, int>(addxy, r);
	int res;
	auto io = na::map_io<int>([&res](int p)->void {
		res = p;
	}, m);
	na::send_event<point>(point(3, 5), r);
	assert(res == 8);
}

static void test_merge_e() {
    printf("%s\n", __FUNCTION__);
    auto r0 = na::receiver_e<point>();
    auto r1 = na::receiver_e<point>();
    auto m = na::merge_e<point>(r0, r1);
    point res;
    auto io = na::map_io<point>([&](point p) {
        res = p;
    }, m);
    na::send_event<point>(point(3, 5), r0);
    assert(res.x == 3);
    assert(res.y == 5);
    na::send_event<point>(point(9, 1), r1);
    assert(res.x == 9);
    assert(res.y == 1);
}

static void test_filter_e() {
    printf("%s\n", __FUNCTION__);
    auto r = na::receiver_e<point>();
    auto m = na::filter_e<point>([=](point p)->bool {
        return p.x < p.y;
    }, r);
    point res;
    auto io = na::map_io<point>([&](point p) {
        res = p;
    }, m);
    na::send_event<point>( point(3, 5), r);
    assert(res.x == 3);
    assert(res.y == 5);
    na::send_event<point>( point(9, 1), r);
    assert(res.x == 3);
    assert(res.y == 5);
}

static void test_not_e() {
    printf("%s\n", __FUNCTION__);
    auto r = na::receiver_e<bool>();
    auto m = na::not_e(r);
    bool res;
    auto io = na::map_io<bool>([&](bool p) {
        res = p;
    }, m);
    na::send_event(true, r);
    assert(!res);
    na::send_event(false, r);
    assert(res);
}

int main(int argc, char *argv[]) {
	test_map_e();
    test_merge_e();
    test_filter_e();
    test_not_e();

	printf("tests passed\n");
}

