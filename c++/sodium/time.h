#ifndef _SODIUM_TIME_AT_H_
#define _SODIUM_TIME_AT_H_

#include <sodium/sodium.h>
#include <memory>
#include <boost/optional.hpp>

namespace sodium {
    /*!
     * A timer that fires at the specified time.
     * The T class provides the implementation of a timer system.
     */
    template <class T, class P = sodium::def_part>
    event<unit, P> at(const T& timeSys, const behavior<boost::optional<typename T::time>>& tAlarm) {
        using namespace boost;
        std::shared_ptr<optional<typename T::timer>> current(new optional<typename T::timer>);
        event_sink<unit, P> eOut;
        auto kill = tAlarm.value().listen([current, eOut, timeSys] (const optional<typename T::time>& ot) {
            if (*current)
                timeSys.cancel_timer(current->get());
            *current = ot ? optional<typename T::timer>(
                                timeSys.set_timer(ot.get(), [eOut] () { eOut.send(unit()); })
                            )
                          : optional<typename T::timer>();
        });
        return eOut.add_cleanup(kill);
    }

    namespace impl {
        // TO DO: Revisit.
        // Ideally the clock time would get frozen at the start of the transaction.
        template <class T>
        struct behavior_impl_time : behavior_impl {
            behavior_impl_time(const T& timeSys) : timeSys(timeSys), now(NULL) {}
            ~behavior_impl_time() { delete now; }
            T timeSys;
            light_ptr* now;
            virtual const light_ptr& sample() const {
                auto me = const_cast<behavior_impl_time<T>*>(this);
                if (now == NULL)
                    me->now = new light_ptr(light_ptr::create<typename T::time>(timeSys.now()));
                else
                    *me->now = light_ptr::create<typename T::time>(timeSys.now());
                return *now;
            }
            virtual const light_ptr& newValue() const { return sample(); }
        };
    }

    /*!
     * A behavior that always has the current clock time.
     */
    template <class T, class P = def_part>
    behavior<typename T::time,P> clock(const T& t)
    {
        return behavior<typename T::time,P>(SODIUM_SHARED_PTR<impl::behavior_impl>(
            new impl::behavior_impl_time<T>(t)
        ));
    }
}  // end namespace sodium
#endif
