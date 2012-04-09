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

static void test_map() {
    auto r = li::receiver_e<point>();
    auto m = li::map_e<point, point>(transpose, r);
    std::shared_ptr<point> res;
    auto io = li::map_io<point>([&](std::shared_ptr<point> p) {
        res = p;}, m);
    li::send_event(std::shared_ptr < point > (new point(3, 5)), r);
    assert(res->x == 5);
    assert(res->y == 3);
}

int main(int argc, char *argv[]) {
    test_map();
    printf("tests passed\n");
}

