/**
 * Copyright (c) 2012-2013, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM_SODIUM_H_
#define _SODIUM_SODIUM_H_

#include <sodium/light_ptr.h>
#include <sodium/transaction.h>
#include <functional>
#include <boost/optional.hpp>
#include <memory>
#include <list>
#include <set>
#include <stdexcept>

#define SODIUM_CONSTANT_OPTIMIZATION

namespace sodium {
    template <class A, class P> class event;
    template <class A, class P> class behavior;
    template <class A, class P> class behavior_sink;
    template <class A, class P> class behavior_loop;
    template <class A, class P> class event_loop;
    template <class A, class B, class P = def_part>
    behavior<B, P> apply(const behavior<std::function<B(const A&)>, P>& bf, const behavior<A, P>& ba);
    template <class A, class P = def_part>
    event<A, P> filter_optional(const event<boost::optional<A>, P>& input);
    template <class A, class P = def_part>
    event<A, P> split(const event<std::list<A>, P>& e);
    template <class A, class P = def_part>
    event<A, P> switch_e(const behavior<event<A, P>, P>& bea);

    namespace impl {

        class behavior_;
        class behavior_impl;

        class event_ {
        friend class behavior_;
        template <class A, class P> friend class sodium::event;
        template <class A, class P> friend class sodium::event_loop;
        template <class A, class P> friend class sodium::behavior;
        friend behavior_ switch_b(transaction_impl* trans, const behavior_& bba);
        friend behavior_impl* hold(transaction_impl* trans0, const light_ptr& initValue, const event_& input);
        template <class A, class B, class P>
        friend behavior<B, P> sodium::apply(const behavior<std::function<B(const A&)>, P>& bf, const behavior<A, P>& ba);
        template <class A, class P>
        friend event<A, P> sodium::filter_optional(const event<boost::optional<A>, P>& input);
        friend behavior_ apply(transaction_impl* trans0, const behavior_& bf, const behavior_& ba);
        friend event_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f, const event_& ev);
        friend event_ switch_e(transaction_impl* trans, const behavior_& bea);
        template <class A, class P>
        friend event<A, P> sodium::split(const event<std::list<A>, P>& e);

        public:
            typedef std::function<void(std::vector<light_ptr>&)> sample_now_func;

        protected:
            boost::intrusive_ptr<listen_impl_func<H_EVENT>> p_listen_impl;
            std::shared_ptr<sample_now_func> p_sample_now;

        public:
            event_();
            event_(const boost::intrusive_ptr<listen_impl_func<H_EVENT>>& p_listen_impl,
                   const std::shared_ptr<sample_now_func>& p_sample_now)
                : p_listen_impl(p_listen_impl), p_sample_now(p_sample_now) {}
            event_(const boost::intrusive_ptr<listen_impl_func<H_EVENT>>& p_listen_impl,
                   sample_now_func* p_sample_now)
                : p_listen_impl(p_listen_impl), p_sample_now(p_sample_now) {}

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            bool is_never() const { return !impl::alive(p_listen_impl); }
#endif

        protected:

            /*!
             * listen to events.
             */
            std::function<void()>* listen_raw(
                        transaction_impl* trans0,
                        const std::shared_ptr<impl::node>& target,
                        std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handle,
                        bool suppressEarlierFirings) const;

            /*!
             * This is far more efficient than add_cleanup because it modifies the event
             * in place.
             */
            event_ unsafe_add_cleanup(std::function<void()>* cleanup)
            {
                boost::intrusive_ptr<listen_impl_func<H_STRONG>> li(
                    reinterpret_cast<listen_impl_func<H_STRONG>*>(p_listen_impl.get()));
                if (cleanup != NULL) {
                    if (alive(li))
                        li->cleanups.push_front(cleanup);
                    else {
                        (*cleanup)();
                        delete cleanup;
                    }
                }
                return *this;
            }

            /*!
             * This is far more efficient than add_cleanup because it modifies the event
             * in place.
             */
            event_ unsafe_add_cleanup(std::function<void()>* cleanup1, std::function<void()>* cleanup2)
            {
                boost::intrusive_ptr<listen_impl_func<H_STRONG>> li(
                    reinterpret_cast<listen_impl_func<H_STRONG>*>(p_listen_impl.get()));
                if (cleanup1 != NULL) {
                    if (alive(li))
                        li->cleanups.push_front(cleanup1);
                    else {
                        (*cleanup1)();
                        delete cleanup1;
                    }
                }
                if (cleanup2 != NULL) {
                    if (alive(li))
                        li->cleanups.push_front(cleanup2);
                    else {
                        (*cleanup2)();
                        delete cleanup2;
                    }
                }
                return *this;
            }

            /*!
             * Create a new event that is like this event but has an extra cleanup.
             */
            event_ add_cleanup_(transaction_impl* trans, std::function<void()>* cleanup) const;
            behavior_ hold_(transaction_impl* trans, const light_ptr& initA) const;
            event_ once_(transaction_impl* trans) const;
            event_ merge_(transaction_impl* trans, const event_& other) const;
            event_ coalesce_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const;
            event_ last_firing_only_(transaction_impl* trans) const;
            event_ snapshot_(transaction_impl* trans, const behavior_& beh, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const;
            event_ filter_(transaction_impl* trans, const std::function<bool(const light_ptr&)>& pred) const;

            void sample_now(std::vector<light_ptr>& values) const {
                if (p_sample_now != NULL)
                    (*p_sample_now)(values);
            }

            std::function<void()>* listen_impl(
                transaction_impl* trans,
                const std::shared_ptr<impl::node>& target,
                std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handler,
                bool suppressEarlierFirings) const
            {
                boost::intrusive_ptr<listen_impl_func<H_STRONG>> li(
                    reinterpret_cast<listen_impl_func<H_STRONG>*>(p_listen_impl.get()));
                if (alive(li))
                    return (*li->func)(trans, target, handler, suppressEarlierFirings);
                else {
                    delete handler;
                    return NULL;
                }
            }
        };
        #define SODIUM_DETYPE_FUNCTION1(A,B,f) \
                   [f] (const light_ptr& a) -> light_ptr { \
                        return light_ptr::create<B>(f(*a.cast_ptr<A>(NULL))); \
                   }

        event_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f, const event_& ca);

        /*!
         * Function to push a value into an event
         */
        void send(const std::shared_ptr<node>& n, transaction_impl* trans, const light_ptr& ptr);

        /*!
         * Creates an event, that values can be pushed into using impl::send(). 
         */
        std::tuple<
                event_,
                std::shared_ptr<node>
            > unsafe_new_event(event_::sample_now_func* sample_now_func = NULL);

        struct behavior_impl {
            behavior_impl(const light_ptr& constant);
            behavior_impl(
                const event_& changes,
                const std::function<light_ptr()>& sample,
                std::function<void()>* kill,
                const std::shared_ptr<behavior_impl>& parent);
            ~behavior_impl();

            event_ changes;  // Having this here allows references to behavior to keep the
                             // underlying event's cleanups alive, and provides access to the
                             // underlying event, for certain primitives.

            std::function<light_ptr()> sample;
            std::function<void()>* kill;
            std::shared_ptr<behavior_impl> parent;

            std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                             const std::function<void(transaction_impl*, const light_ptr&)>&)> listen_value_raw() const;
        };

        behavior_impl* hold(transaction_impl* trans0,
                            const light_ptr& initValue,
                            const event_& input);

        struct behavior_state {
            behavior_state(const light_ptr& initA);
            ~behavior_state();
            light_ptr current;
            boost::optional<light_ptr> update;
        };

        class behavior_ {
            friend impl::event_ underlying_event(const behavior_& beh);
            public:
                behavior_();
                behavior_(behavior_impl* impl);
                behavior_(const std::shared_ptr<behavior_impl>& impl);
                behavior_(const light_ptr& a);
                std::shared_ptr<impl::behavior_impl> impl;

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                /*!
                 * For optimization, if this behavior is a constant, then return its value.
                 */
                boost::optional<light_ptr> get_constant_value() const;
#endif

                event_ values_(transaction_impl* trans) const;
                const event_& changes_() const { return impl->changes; }
        };

        behavior_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f,
            const behavior_& beh);

        template <class S>
        struct collect_state {
            collect_state(const S& s) : s(s) {}
            S s;
        };
    }  // end namespace impl

    template <class A, class P>
    class event;

    /*!
     * A like an event, but it tracks the input event's current value and causes it
     * always to be output once at the beginning for each listener.
     */
    template <class A, class P = def_part>
    class behavior : protected impl::behavior_ {
        template <class AA, class PP> friend class event;
        template <class AA, class PP> friend class behavior;
        template <class AA, class PP> friend class behavior_loop;
        template <class AA, class BB, class PP>
        friend behavior<BB, PP> apply(const behavior<std::function<BB(const AA&)>, PP>& bf, const behavior<AA, PP>& ba);
        template <class AA, class PP>
        friend behavior<AA, PP> switch_b(const behavior<behavior<AA, PP>, PP>& bba);
        template <class AA, class PP>
        friend event<AA, PP> switch_e(const behavior<event<AA, PP>, PP>& bea);
        private:
            behavior(const std::shared_ptr<impl::behavior_impl>& impl)
                : impl::behavior_(impl)
            {
            }

        protected:
            behavior() {}
            behavior(const impl::behavior_& beh) : impl::behavior_(beh) {}

        public:
            /*!
             * Constant value.
             */
            behavior(const A& a)
                : impl::behavior_(light_ptr::create<A>(a))
            {
            }

            /*!
             * Sample the value of this behavior.
             */
            A sample() const {
                return *impl->sample().template cast_ptr<A>(NULL);
            }

            /*!
             * Returns a new behavior with the specified cleanup added to it, such that
             * it will be executed when no copies of the new behavior are referenced.
             */
            behavior<A, P> add_cleanup(const std::function<void()>& cleanup) const {
                return behavior<A, P>(new impl::behavior_impl(
                    impl->changes,
                    impl->sample,
                    new std::function<void()>(cleanup),
                    impl
                ));
            }

            /*!
             * Map a function over this behaviour to modify the output value.
             */
            template <class B>
            behavior<B, P> map(const std::function<B(const A&)>& f) const {
                transaction<P> trans;
                return behavior<B, P>(impl::map_(trans.impl(), SODIUM_DETYPE_FUNCTION1(A,B,f), *this));
            }

            /*!
             * Map a function over this behaviour to modify the output value.
             *
             * g++-4.7.2 has a bug where, under a 'using namespace std' it will interpret
             * b.template map<A>(f) as if it were std::map. If you get this problem, you can
             * work around it with map_.
             */
            template <class B>
            behavior<B, P> map_(const std::function<B(const A&)>& f) const {
                transaction<P> trans;
                return behavior<B, P>(impl::map_(trans.impl(), SODIUM_DETYPE_FUNCTION1(A,B,f), *this));
            }

            /*!
             * Returns an event describing the changes in a behavior.
             */
            event<A, P> changes() const {
                return event<A, P>(impl->changes);
            }

            /*!
             * Returns an event describing the value of a behavior, where there's an initial event
             * giving the current value.
             */
            event<A, P> values() const {
                transaction<P> trans;
                return event<A, P>(values_(trans.impl()));
            }
        
            /**
             * Transform a behavior with a generalized state loop (a mealy machine). The function
             * is passed the input and the old state and returns the new state and output value.
             */
            template <class S, class B>
            behavior<B, P> collect(
                const S& initS,
                const std::function<std::tuple<B, S>(const A&, const S&)>& f
            ) const
            {
                transaction<P> trans;
                auto ea = changes().coalesce([] (const A&, const A& snd) -> A { return snd; });
                auto za = sample();
                auto zbs = f(za, initS);
                std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(std::get<1>(zbs)));
                auto p = impl::unsafe_new_event();
                auto kill = changes().listen_raw(trans.impl(), std::get<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            auto outsSt = f(*ptr.cast_ptr<A>(NULL), pState->s);
                            pState->s = std::get<1>(outsSt);
                            send(target, trans, light_ptr::create<B>(std::get<0>(outsSt)));
                        }), false);
                return event<B, P>(std::get<0>(p).unsafe_add_cleanup(kill)).hold(std::get<0>(zbs));
            }

    };  // end class behavior

    template <class A, class P = def_part>
    class event : protected impl::event_ {
        template <class AA, class PP> friend class event;
        template <class AA, class PP> friend class event_sink;
        template <class AA, class PP> friend class behavior;
        template <class AA, class PP> friend class behavior_sink;
        template <class AA, class PP> friend class behavior_loop;
        template <class AA, class PP> friend class event_sink;
        template <class AA, class PP> friend event<AA, PP> filter_optional(const event<boost::optional<AA>, PP>& input);
        template <class AA, class PP> friend event<AA, PP> switch_e(const behavior<event<AA, PP>, PP>& bea);
        template <class AA, class PP> friend event<AA, PP> split(const event<std::list<AA>, PP>& e);
        template <class AA, class PP> friend class sodium::event_loop;
        public:
            /*!
             * The 'never' event (that never fires).
             */
            event() {}
        protected:
            event(const impl::listen_impl_func<impl::H_EVENT>& listen)
                : impl::event_(listen, new impl::event_::sample_now_func(NULL)) {}
            event(const impl::event_& ev) : impl::event_(ev) {}
        public:
            /*!
             * High-level interface to obtain an event's value.
             */
            std::function<void()> listen(std::function<void(const A&)> handle) const {
                transaction<P> trans;
                std::function<void()>* pKill = listen_raw(trans.impl(),
                    std::shared_ptr<impl::node>(new impl::node(SODIUM_IMPL_RANK_T_MAX)),
                    new std::function<void(const std::shared_ptr<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [handle] (const std::shared_ptr<impl::node>&, impl::transaction_impl* trans, const light_ptr& ptr) {
                            handle(*ptr.cast_ptr<A>(NULL));
                        }), false);
                if (pKill != NULL) {
                    std::function<void()> kill(*pKill);
                    delete pKill;
                    return kill;
                }
                else
                    return [] () {};
            };

            /*!
             * Map a function over this event to modify the output value.
             */
            template <class B>
            event<B, P> map(const std::function<B(const A&)>& f) const {
                transaction<P> trans;
                return event<B, P>(impl::map_(trans.impl(), SODIUM_DETYPE_FUNCTION1(A,B,f), *this));
            }
    
            /*!
             * Map a function over this event to modify the output value.
             *
             * g++-4.7.2 has a bug where, under a 'using namespace std' it will interpret
             * b.template map<A>(f) as if it were std::map. If you get this problem, you can
             * work around it with map_.
             */
            template <class B>
            event<B, P> map_(const std::function<B(const A&)>& f) const {
                return event<B, P>(impl::map_(SODIUM_DETYPE_FUNCTION1(A,B,f), *this));
            }

            /*!
             * Merge two events. If both events fire in the same transaction, then the
             * output event will just give two firings within the same transaction.
             * This is allowed, but it is usually best avoided.
             */
            event<A, P> merge(const event<A, P>& other) const {
                transaction<P> trans;
                return event<A, P>(merge_(trans.impl(), other));
            }

            /*!
             * If there's more than one firing in a single transaction, combine them into
             * one using the specified combining function.
             *
             * If the event firings are ordered, then the first will appear at the left
             * input of the combining function. In most common cases it's best not to
             * make any assumptions about the ordering, and the combining function would
             * ideally be commutative.
             */
            event<A, P> coalesce(const std::function<A(const A&, const A&)>& combine) const
            {
                transaction<P> trans;
                return event<A, P>(coalesce_(trans.impl(), [combine] (const light_ptr& a, const light_ptr& b) -> light_ptr {
                    return light_ptr::create<A>(combine(*a.cast_ptr<A>(NULL), *b.cast_ptr<A>(NULL)));
                }));
            }

            /*!
             * Merge two streams of events of the same type, combining simultaneous
             * event occurrences.
             *
             * In the case where multiple event occurrences are simultaneous (i.e. all
             * within the same transaction), they are combined using the same logic as
             * 'coalesce'.
             */
            event<A, P> merge(const event<A, P>& other, const std::function<A(const A&, const A&)>& combine) const
            {
                return merge(other).coalesce(combine);
            }

            /*!
             * Filter this event based on the specified predicate, passing through values
             * where the predicate returns true.
             */
            event<A, P> filter(const std::function<bool(const A&)>& pred) const
            {
                transaction<P> trans;
                return event<A, P>(filter_(trans.impl(), [pred] (const light_ptr& a) {
                    return pred(*a.cast_ptr<A>(NULL));
                }));
            }

            /*!
             * Create a behavior that holds at any given time the most recent value
             * that has arrived from this event. Since behaviors must always have a current
             * value, you must supply an initial value that it has until the first event
             * occurrence updates it.
             */
            behavior<A, P> hold(const A& initA) const
            {
                transaction<P> trans;
                return behavior<A, P>(hold_(trans.impl(), light_ptr::create<A>(initA)));
            }

            /*!
             * Sample the behavior's value as at the transaction before the
             * current one, i.e. no changes from the current transaction are
             * taken.
             */
            template <class B, class C>
            event<C, P> snapshot(const behavior<B, P>& beh, const std::function<C(const A&, const B&)>& combine) const
            {
                transaction<P> trans;
                return event<C, P>(snapshot_(trans.impl(), beh, [combine] (const light_ptr& a, const light_ptr& b) -> light_ptr {
                    return light_ptr::create<C>(combine(*a.cast_ptr<A>(NULL), *b.cast_ptr<B>(NULL)));
                }));
            }

            /*!
             * Sample the behavior's value as at the transaction before the
             * current one, i.e. no changes from the current transaction are
             * taken.
             */
            template <class B>
            event<B, P> snapshot(const behavior<B, P>& beh) const
            {
                return snapshot<B, B>(beh, [] (const A&, const B& b) { return b; });
            }

            /*!
             * Allow events through only when the behavior's value is true.
             */
            event<A, P> gate(const behavior<bool, P>& g) const
            {
                transaction<P> trans;
                return filter_optional<A>(snapshot<bool, boost::optional<A>>(
                    g,
                    [] (const A& a, const bool& gated) {
                        return gated ? boost::optional<A>(a) : boost::optional<A>();
                    })
                );
            }

            /*!
             * Adapt an event to a new event statefully.  Always outputs one output for each
             * input.
             */
            template <class S, class B>
            event<B, P> collect(
                const S& initS,
                const std::function<std::tuple<B, S>(const A&, const S&)>& f
            ) const
            {
                transaction<P> trans;
                std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(initS));
                auto p = impl::unsafe_new_event();
                auto kill = listen_raw(trans.impl(), std::get<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            auto outsSt = f(*ptr.cast_ptr<A>(NULL), pState->s);
                            pState->s = std::get<1>(outsSt);
                            send(target, trans, light_ptr::create<B>(std::get<0>(outsSt)));
                        }), false);
                return std::get<0>(p).unsafe_add_cleanup(kill);
            }

            template <class B>
            behavior<B, P> accum(
                const B& initB,
                const std::function<B(const A&, const B&)>& f
            ) const
            {
                transaction<P> trans;
                std::shared_ptr<impl::collect_state<B>> pState(new impl::collect_state<B>(initB));
                auto p = impl::unsafe_new_event();
                auto kill = listen_raw(trans.impl(), std::get<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            pState->s = f(*ptr.cast_ptr<A>(NULL), pState->s);
                            send(target, trans, light_ptr::create<B>(pState->s));
                        }), false);
                return event<B, P>(std::get<0>(p).unsafe_add_cleanup(kill)).hold(initB);
            }

            behavior<int, P> count() const
            {
                return accum<int>(0, [] (const A&, const int& total) -> int {
                    return total+1;
                });
            }

            event<A, P> once() const
            {
                transaction<P> trans;
                return event<A, P>(once_(trans.impl()));
            }

            /*!
             * Add a clean-up operation to be performed when this event is no longer
             * referenced.
             */
            event<A, P> add_cleanup(const std::function<void()>& cleanup) const
            {
                transaction<P> trans;
                return event<A, P>(add_cleanup_(trans.impl(), new std::function<void()>(cleanup)));
            }
    };  // end class event

    namespace impl {
        struct event_sink_impl {
            event_sink_impl();
            event_ construct();
            void send(transaction_impl* trans, const light_ptr& ptr) const;
            std::shared_ptr<impl::node> target;
        };
    }

    /*!
     * An event with a send() method to allow values to be pushed into it
     * from the imperative world.
     */
    template <class A, class P = def_part>
    class event_sink : public event<A, P>
    {
        private:
            impl::event_sink_impl impl;
            event_sink(const impl::event_& e) : event<A, P>(e) {}

        public:
            event_sink()
            {
                *reinterpret_cast<event<A,P>*>(this) = impl.construct();
            }

            void send(const A& a) const {
                transaction<P> trans;
                impl.send(trans.impl(), light_ptr::create<A>(a));
            }
    };

    /*!
     * Make the specified event cross to partition Q.
     */
    template <class A, class P, class Q>
    event<A, Q> cross(const event<A, P>& e)
    {
        transaction<P> trans;
        event_sink<A, Q> s;
        auto kill = e.listen([s] (const A& a) {
            transaction<P> trans;
            trans.impl()->part->post([s, a] () {
                s.send(a);
            });
        });
        return s.add_cleanup(kill);
    }

    /*!
     * Make the specified behavior cross to partition Q.
     */
    template <class A, class P, class Q>
    behavior<A, Q> cross(const behavior<A, P>& b)
    {
        transaction<P> trans;
        return cross<A, P, Q>(b.changes()).hold(b.sample());
    }

    /*!
     * Filter an event of optionals, keeping only the defined values.
     */
    template <class A, class P>
    event<A, P> filter_optional(const event<boost::optional<A>, P>& input)
    {
        transaction<P> trans;
        auto p = impl::unsafe_new_event();
        auto kill = input.listen_raw(trans.impl(), std::get<1>(p),
            new std::function<void(const std::shared_ptr<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                [] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& poa) {
                    const boost::optional<A>& oa = *poa.cast_ptr<boost::optional<A>>(NULL);
                    if (oa) impl::send(target, trans, light_ptr::create<A>(oa.get()));
                }), false);
        return std::get<0>(p).unsafe_add_cleanup(kill);
    }

    /*!
     * A behavior with a send() method to allow its value to be changed
     * from the imperative world.
     */
    template <class A, class P = def_part>
    class behavior_sink : public behavior<A, P>
    {
        private:
            event_sink<A, P> e;

            behavior_sink(const behavior<A, P>& beh) : behavior<A, P>(beh) {}

        public:
            behavior_sink(const A& initA)
            {
                transaction<P> trans;
                this->impl = std::shared_ptr<impl::behavior_impl>(hold(trans.impl(), light_ptr::create<A>(initA), e));
            }

            void send(const A& a) const
            {
                e.send(a);
            }
    };

    namespace impl {
        /*!
         * Returns an event describing the changes in a behavior.
         */
        inline impl::event_ underlying_event(const impl::behavior_& beh) {return beh.impl->changes;}
    };

    namespace impl {
        behavior_ apply(transaction_impl* trans, const behavior_& bf, const behavior_& ba);
    };

    /*!
     * Apply a function contained in a behavior to a behavior value. This is the primitive
     * for all lifting of functions into behaviors.
     */
    template <class A, class B, class P>
    behavior<B, P> apply(const behavior<std::function<B(const A&)>, P>& bf, const behavior<A, P>& ba)
    {
        transaction<P> trans;
        return behavior<B, P>(impl::apply(
            trans.impl(),
            impl::map_(trans.impl(), [] (const light_ptr& pf) -> light_ptr {
                const std::function<B(const A&)>& f = *pf.cast_ptr<std::function<B(const A&)>>(NULL);
                return light_ptr::create<std::function<light_ptr(const light_ptr&)>>(
                        SODIUM_DETYPE_FUNCTION1(A, B, f)
                    );
            }, bf),
            ba
        ));
    }

    /*!
     * Enable the construction of event loops, like this. This gives the ability to
     * forward reference an event.
     *
     *   event_loop<A> ea;
     *   auto ea_out = do_something(ea);
     *   ea.loop(ea_out);  // ea is now the same as ea_out
     *
     * TO DO: Loops do not yet get deallocated properly.
     */
    template <class A, class P = def_part>
    class event_loop : public event<A, P>
    {
        private:
            struct info {
                info(
                    const std::shared_ptr<impl::node>& target,
                    const std::shared_ptr<std::function<void()>*>& pKill
                )
                : target(target), pKill(pKill)
                {
                }
                std::shared_ptr<impl::node> target;
                std::shared_ptr<std::function<void()>*> pKill;
            };
            std::shared_ptr<info> i;

        private:
            event_loop(const impl::event_& ev, const std::shared_ptr<info>& i) : event<A, P>(ev), i(i) {}

        public:
            event_loop()
            {
                std::shared_ptr<std::function<void()>*> pKill(
                    new std::function<void()>*(new std::function<void()>(
                        [] () {
                            throw std::runtime_error("event_loop not looped back");
                        }
                    ))
                );
                auto p = impl::unsafe_new_event();
                *this = event_loop<A>(
                    std::get<0>(p).unsafe_add_cleanup(new std::function<void()>([pKill] () {
                        std::function<void()>* kill = *pKill;
                        if (kill)
                            (*kill)();
                        delete kill;
                    })),
                    std::shared_ptr<info>(new info(std::get<1>(p), pKill))
                );
            }

            void loop(const event<A, P>& e)
            {
                if (i) {
                    transaction<P> trans;
                    auto target(i->target);
                    *i->pKill = e.listen_raw(trans.impl(), target, NULL, false);
                    i = std::shared_ptr<info>();
                }
                else
                    throw std::runtime_error("event_loop looped back more than once");
            }
    };

    /*!
     * Enable the construction of behavior loops, like this. This gives the ability to
     * forward reference a behavior.
     *
     *   behavior_loop<A> ba;
     *   auto ba_out = do_something(ea);
     *   ea.loop(ba_out);  // ba is now the same as ba_out
     *
     * TO DO: Loops do not yet get deallocated properly.
     */
    template <class A, class P = def_part>
    class behavior_loop : public behavior<A, P>
    {
        private:
            event_loop<A> elp;
            std::shared_ptr<std::function<light_ptr()>> pSample;

        public:
            behavior_loop()
                : behavior<A, P>(impl::behavior_()),
                  pSample(new std::function<light_ptr()>([] () -> light_ptr {
                      throw std::runtime_error("behavior_loop sampled before it was looped");
                  }))
            {
                auto pSample = this->pSample;
                this->impl = std::shared_ptr<impl::behavior_impl>(new impl::behavior_impl(
                    elp,
                    [pSample] () { return (*pSample)(); },
                    NULL,
                    std::shared_ptr<impl::behavior_impl>()));
            }

            void loop(const behavior<A, P>& b)
            {
                elp.loop(b.changes());
                *pSample = b.impl->sample;
                // TO DO: This keeps the memory allocated in a loop. Figure out how to
                // break the loop.
                this->impl->parent = b.impl;
            }
    };

    namespace impl {
        event_ switch_e(transaction_impl* trans, const behavior_& bea);
    }

    /*!
     * Flatten a behavior that contains an event to give an event that reflects
     * the current state of the behavior. Note that when an event is updated,
     * due to behavior's delay semantics, event occurrences for the new
     * event won't come through until the following transaction.
     */
    template <class A, class P = def_part>
    event<A, P> switch_e(const behavior<event<A, P>, P>& bea)
    {
        transaction<P> trans;
        return event<A, P>(impl::switch_e(trans.impl(), bea));
    }

    namespace impl {
        behavior_ switch_b(transaction_impl* trans, const behavior_& bba);
    }

    /*!
     * Behavior variant of switch.
     */
    template <class A, class P = def_part>
    behavior<A, P> switch_b(const behavior<behavior<A, P>, P>& bba)
    {
        transaction<P> trans;
        return behavior<A, P>(impl::switch_b(trans.impl(), bba));
    }

    /*!
     * Lift a binary function into behaviors.
     */
    template <class A, class B, class C, class P = def_part>
    behavior<C, P> lift(const std::function<C(const A&, const B&)>& f, const behavior<A, P>& ba, const behavior<B, P>& bb)
    {
        std::function<std::function<C(const B&)>(const A&)> fa(
            [f] (const A& a) -> std::function<C(const B&)> {
                return [f, a] (const B& b) -> C { return f(a, b); };
            }
        );
        transaction<P> trans;
        return apply<B, C>(ba.map_(fa), bb);
    }

    /*!
     * Lift a ternary function into behaviors.
     */
    template <class A, class B, class C, class D, class P = def_part>
    behavior<D, P> lift(const std::function<D(const A&, const B&, const C&)>& f,
        const behavior<A, P>& ba,
        const behavior<B, P>& bb,
        const behavior<C, P>& bc
    )
    {
        std::function<std::function<std::function<D(const C&)>(const B&)>(const A&)> fa(
            [f] (const A& a) -> std::function<std::function<D(const C&)>(const B&)> {
                return [f, a] (const B& b) -> std::function<D(const C&)> {
                    return [f, a, b] (const C& c) -> D {
                        return f(a,b,c);
                    };
                };
            }
        );
        return apply(apply(ba.map_(fa), bb), bc);
    }

    /*!
     * Lift a quaternary function into behaviors.
     */
    template <class A, class B, class C, class D, class E, class P = def_part>
    behavior<E, P> lift(const std::function<E(const A&, const B&, const C&, const D&)>& f,
        const behavior<A, P>& ba,
        const behavior<B, P>& bb,
        const behavior<C, P>& bc,
        const behavior<D, P>& bd
    )
    {
        std::function<std::function<std::function<std::function<E(const D&)>(const C&)>(const B&)>(const A&)> fa(
            [f] (const A& a) -> std::function<std::function<std::function<E(const D&)>(const C&)>(const B&)> {
                return [f, a] (const B& b) -> std::function<std::function<E(const D&)>(const C&)> {
                    return [f, a, b] (const C& c) -> std::function<E(const D&)> {
                        return [f, a, b, c] (const D& d) -> E {
                            return f(a,b,c,d);
                        };
                    };
                };
            }
        );
        return apply(apply(apply(ba.map_(fa), bb), bc), bd);
    }

    /*!
     * Take each list item and put it into a new transaction of its own.
     *
     * An example use case of this might be a situation where we are splitting
     * a block of input data into frames. We obviously want each frame to have
     * its own transaction so that state is updated separately for each frame.
     */
    template <class A, class P>
    event<A, P> split(const event<std::list<A>, P>& e)
    {
        auto p = impl::unsafe_new_event();
        transaction<P> trans;
        auto kill = e.listen_raw(trans.impl(), std::get<1>(p),
            new std::function<void(const std::shared_ptr<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                [] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                    const std::list<A>& la = *ptr.cast_ptr<std::list<A>>(NULL);
                    trans->part->post([la, target, ptr] () {
                        for (auto it = la.begin(); it != la.end(); ++it) {
                            transaction<P> trans;
                            send(target, trans.impl(), light_ptr::create<A>(*it));
                        }
                    });
                }), false);
        return std::get<0>(p).unsafe_add_cleanup(kill);
    }

}  // end namespace sodium
#endif

