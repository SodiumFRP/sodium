/*
 * lithium.c
 *
 *  Created on: 8/04/2012
 *      Author: Anthony Jones <ajones@clear.net.nz>
 */

#include <queue>
#include "sodium.h"

namespace na {

rank_t generic_event::next_rank;

auto generic_event::propagate_pulse(generic_pulse pulse) -> void {
	pulse_queue queue;
	queue->push(pulse);
	while (!queue->empty()) {
		pulse = queue->top();
		queue->pop();
		pulse.notify(queue);
	}
}

auto not_e(event<bool> source) -> event<bool> {
	return map_e<bool, bool>([=](bool b) -> bool {return !b;}, source);
}

}
