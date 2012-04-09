/*
 * lithium.c
 *
 *  Created on: 8/04/2012
 *      Author: Anthony Jones <ajones@clear.net.nz>
 */

#ifndef LITHIUM_H_
#define LITHIUM_H_

#include <functional>
#include <memory>
#include <list>

namespace li {
class unit {
};

namespace impl {
class base;
typedef std::shared_ptr<base> base_ptr;
typedef std::shared_ptr<void> data;

auto propagate_pulse(base_ptr self, data) -> void;
auto listen_to(base_ptr self, base_ptr source) -> void;
auto receiver_e()->base_ptr;
auto map_e(std::function<auto(data) -> data> f, base_ptr source)->base_ptr;
auto map_io(std::function<auto(data) -> void> f, base_ptr source)->base_ptr;
}

template<class T>
class event {
public:

    event(impl::base_ptr impl) :
            impl(impl) {
    }
    impl::base_ptr impl;
};

template<class T>
auto receiver_e() -> event<T> {
    return event<T>(impl::receiver_e());
}

template<class T>
auto send_event(std::shared_ptr<T> value, event<T> source) -> void {
    impl::propagate_pulse(source.impl, value);
}

template<class A, class T>
auto map_e(std::function<auto(std::shared_ptr<A>) -> std::shared_ptr<T>> f,
        event<A> source) -> event<T> {
    return event<T>(
            impl::map_e(
                    [=](std::shared_ptr<void> value)->std::shared_ptr<void> {
                        std::shared_ptr<T> ret = f(*reinterpret_cast<std::shared_ptr<A>*>(&value));
                        return *reinterpret_cast<std::shared_ptr<void>*>(&ret);
                    }, source.impl));
}

template<class A>
auto map_io(std::function<auto(std::shared_ptr<A>) -> void> f,
        event<A> source) -> event<unit> {
    return event<unit>(impl::map_io([=](std::shared_ptr<void> value)->void {
        f(*reinterpret_cast<std::shared_ptr<A>*>(&value));
    }, source.impl));
}

}

#endif
