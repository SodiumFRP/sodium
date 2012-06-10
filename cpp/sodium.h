/*
 * lithium.c
 *
 *  Created on: 8/04/2012
 *      Author: Anthony Jones <ajones@clear.net.nz>
 */

#ifndef SODIUM_H_
#define SODIUM_H_

#include <assert.h>
#include <functional>
#include <memory>
#include <list>
#include <queue>

namespace na {
class unit {
};

typedef long long rank_t;

class pulse_queue;

class generic_pulse {
public:
	generic_pulse(rank_t rank,
			std::function<auto(pulse_queue&) -> void> notify_function) :
			rank(rank), notify_function(notify_function) {
	}
	auto operator<(const generic_pulse& other) const -> bool {
		return rank < other.rank;
	}
	auto notify(pulse_queue& queue) -> void {
		notify_function(queue);
	}
private:
	rank_t rank;
	std::function<auto(pulse_queue&) -> void> notify_function;
};

class generic_event {
public:
	generic_event() {
		rank = ++next_rank;
	}
	auto get_rank() -> rank_t {
		return rank;
	}
	static auto propagate_pulse(generic_pulse pulse) -> void;
private:
	rank_t rank;
	static rank_t next_rank;
};

template<typename T>
class event;

template<typename T>
class listener {
public:
	listener(std::function<auto(void)->rank_t> get_rank_function,
			std::function<auto(T, pulse_queue&)->void> notify_function) :
			get_rank_function(get_rank_function), notify_function(
					notify_function) {
	}
	auto notify(T value, pulse_queue& queue) const ->void {
		notify_function(value, queue);
	}
	virtual auto get_rank() const -> rank_t {
		return get_rank_function();
	}
private:
	std::function<auto(void)->rank_t> get_rank_function;
	std::function<auto(T, pulse_queue&)->void> notify_function;
};

class pulse_queue {
public:
	auto operator->() -> std::priority_queue<generic_pulse>* {
		return &queue;
	}
private:
	std::priority_queue<generic_pulse> queue;
};

template<typename T>
class impl_event: public generic_event {
public:
	impl_event() {
	}
	auto listen_to(listener<T> l) -> void {
		sends_to.push_back(l);
	}
	auto notify_all(T value, pulse_queue& queue) -> void {
		for (auto itr = sends_to.begin(); itr != sends_to.end(); itr++) {
			listener<T> lst = *itr;
			queue->push(generic_pulse(lst.get_rank(), [=,&queue](pulse_queue&) {
				lst.notify(value, queue);
			}));
		}
	}
private:
	std::list<listener<T>> sends_to;
};

template<typename T>
class event {
public:
	event() :
			impl(new impl_event<T>) {
	}
	auto operator*() const -> impl_event<T>& {
		return *impl;
	}
	auto operator->() const -> impl_event<T>* {
		return &(*impl);
	}
private:
	std::shared_ptr<impl_event<T>> impl;
};

class io {
public:
	io() :
			impl(new generic_event) {
	}
	auto operator->() const -> generic_event* {
		return &(*impl);
	}
private:
	std::shared_ptr<generic_event> impl;
};

template<typename T>
auto receiver_e() -> event<T> {
	return event<T>();
}

template<typename T>
auto send_event(T value, event<T> source) -> void {
	auto pulse = generic_pulse(source->get_rank(), [=](pulse_queue& queue) {
		source->notify_all(value, queue);
	});
	generic_event::propagate_pulse(pulse);
}

template<typename S, typename R>
auto map_e(std::function<auto(S) -> R> function, event<S> source) -> event<R> {
	auto map = event<R>();
	auto get_rank = [=] {return map->get_rank();};
	auto f = [=](S value, pulse_queue& queue)->void {
		map->notify_all(function(value), queue);
	};
	source->listen_to(listener<S>(get_rank, f));
	return map;
}

template<typename T>
auto map_io(std::function<auto(T) ->void> function, event<T> source) -> io {
	auto result = io();
	auto f = [=](T value, pulse_queue queue) {
		function(value);
	};
	source->listen_to(listener<T>([=] {return result->get_rank();}, f));
	return result;
}

template<typename T>
auto merge_e(event<T> source0, event<T> source1) -> event<T> {
	auto merge = event<T>();
	auto get_rank = [=] {return merge->get_rank();};
	auto f = [=](T value, pulse_queue& queue)->void {
		merge->notify_all(value, queue);
	};
	source0->listen_to(listener<T>(get_rank, f));
	source1->listen_to(listener<T>(get_rank, f));
	return merge;
}

template<typename T>
auto filter_e(std::function<auto(T) -> bool> function,
		event<T> source) -> event<T> {
	auto filter = event<T>();
	auto get_rank = [=] {return filter->get_rank();};
	auto f = [=](T value, pulse_queue& queue) {
		if (function(value)) {
			filter->notify_all(value, queue);
		}
	};
	source->listen_to(listener<T>(get_rank, f));
	return filter;
}

template<typename S, typename R>
auto constant_e(R value, event<S> source) -> event<S> {
	return map_e([=](S source) {return value;}, source);
}

// Non-template methods
auto not_e(event<bool> source) -> event<bool>;

}

#endif
