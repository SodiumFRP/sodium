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

static auto transpose(point p) -> point {
	return point(p.y, p.x);
}

static void test_map_e() {
	printf("%s\n", __FUNCTION__);
	auto r = na::receiver_e<point>();
	auto m = na::map_e<point, point>(transpose, r);
	point res;
	auto io = na::map_io<point>([&res](point p)->void {
		res = p;
	}, m);
	na::send_event<point>(point(3, 5), r);
	assert(res.x == 5);
	assert(res.y == 3);
}

//static void test_merge_e() {
//    printf("%s\n", __FUNCTION__);
//    auto r0 = na::receiver_e<point>();
//    auto r1 = na::receiver_e<point>();
//    auto m = na::merge_e<point>(r0, r1);
//    std::shared_ptr<point> res;
//    auto io = na::map_io<point>([&](std::shared_ptr<point> p) {
//        res = p;
//    }, m);
//    na::send_event(std::shared_ptr < point > (new point(3, 5)), r0);
//    assert(res->x == 3);
//    assert(res->y == 5);
//    na::send_event(std::shared_ptr < point > (new point(9, 1)), r1);
//    assert(res->x == 9);
//    assert(res->y == 1);
//}
//
//static void test_filter_e() {
//    printf("%s\n", __FUNCTION__);
//    auto r = na::receiver_e<point>();
//    auto m = na::filter_e<point>(r, [=](std::shared_ptr<point> p)->bool {
//        return p->x < p->y;
//    });
//    std::shared_ptr<point> res;
//    auto io = na::map_io<point>([&](std::shared_ptr<point> p) {
//        res = p;
//    }, m);
//    na::send_event(std::shared_ptr < point > (new point(3, 5)), r);
//    assert(res->x == 3);
//    assert(res->y == 5);
//    na::send_event(std::shared_ptr < point > (new point(9, 1)), r);
//    assert(res->x == 3);
//    assert(res->y == 5);
//}
//
//static void test_not_e() {
//    printf("%s\n", __FUNCTION__);
//    auto r = na::receiver_e<bool>();
//    auto m = na::not_e(r);
//    std::shared_ptr<bool> res;
//    auto io = na::map_io<bool>([&](std::shared_ptr<bool> p) {
//        res = p;
//    }, m);
//    na::send_event(std::shared_ptr <bool> (new bool(true)), r);
//    assert(!*res);
//    na::send_event(std::shared_ptr <bool> (new bool(false)), r);
//    assert(*res);
//}

int main(int argc, char *argv[]) {
	test_map_e();
//    test_merge_e();
//    test_filter_e();
//    test_not_e();

	printf("tests passed\n");
}

