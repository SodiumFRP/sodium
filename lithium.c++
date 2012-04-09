/*
 * lithium.c
 *
 *  Created on: 8/04/2012
 *      Author: Anthony Jones <ajones@clear.net.nz>
 */

#include <queue>
#include "lithium.h"

namespace li {

namespace impl {

class base {
public:
    base(std::function<auto(data)->data> update);
    long rank;
    std::function<auto(data)->data> update;
    std::list<base_ptr> sends_to;
    std::list<base_ptr> listening_to;

    static long next_rank;

    friend class comparison;
};

class queue_event {
public:
    queue_event(base_ptr dest, data value) :
            dest(dest), value(value) {
    }
    base_ptr dest;
    data value;
};

class comparison {
public:
    bool operator()(const queue_event& lhs, const queue_event& rhs) const {
        return lhs.dest->rank < rhs.dest->rank;
    }
};

long base::next_rank;

base::base(std::function<auto(data)->data> update) :
        rank(next_rank++), update(update) {
}

auto listen_to(base_ptr self, base_ptr source) -> void {
    self->listening_to.push_back(source);
    source->sends_to.push_back(self);
}

auto propagate_pulse(base_ptr self, data v) -> void {
    std::priority_queue<queue_event, std::vector<queue_event>, comparison> pq;
    pq.push(queue_event(self, v));
    while (!pq.empty()) {
        queue_event qe = pq.top();
        pq.pop();

        data nextValue = qe.dest->update(qe.value);
        for (auto itr = qe.dest->sends_to.begin();
                itr != qe.dest->sends_to.end(); itr++) {
            pq.push(queue_event(*itr, nextValue));
        }
    }
}

auto receiver_e()->base_ptr {
    return std::shared_ptr < base > (new base([=](data value)->data {
        return value;
    }));
}

auto map_e(std::function<auto(data) -> data> f, base_ptr source)->base_ptr {
    base_ptr self = std::shared_ptr < base > (new base([=](data value)->data {
        return f(value);
    }));
    listen_to(self, source);
    return self;
}

auto map_io(std::function<auto(data) -> void> f, base_ptr source)->base_ptr {
    base_ptr self = std::shared_ptr < base > (new base([=](data value)->data {
        f(value);
        return std::shared_ptr<void>(new unit);
    }));
    listen_to(self, source);
    return self;
}

}
}
