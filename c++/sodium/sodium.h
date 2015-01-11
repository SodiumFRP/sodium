/**
 * Copyright (c) 2012-2014, Stephen Blackheath and Anthony Jones
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
#if defined(SODIUM_NO_EXCEPTIONS)
#include <stdlib.h>
#else
#include <stdexcept>
#endif
#include <vector>

#define SODIUM_CONSTANT_OPTIMIZATION

// TO DO:
// the sample_lazy() mechanism is not correct yet. The lazy value needs to be
// fixed at the end of the transaction.

namespace sodium {
    template <class A, class P> class event;
    template <class A, class P> class behavior;
    template <class A, class P> class behavior_sink;
    template <class A, class P> class behavior_loop;
    template <class A, class P> class event_loop;
    template <class A, class B, class P EQ_DEF_PART>
#if defined(SODIUM_NO_CXX11)
    behavior<B, P> apply(const behavior<lambda1<B,const A&>, P>& bf, const behavior<A, P>& ba);
#else
    behavior<B, P> apply(const behavior<std::function<B(const A&)>, P>& bf, const behavior<A, P>& ba);
#endif
    template <class A, class P EQ_DEF_PART>
    event<A, P> filter_optional(const event<boost::optional<A>, P>& input);
    template <class A, class P EQ_DEF_PART>
    event<A, P> split(const event<std::list<A>, P>& e);
    template <class A, class P EQ_DEF_PART>
    event<A, P> switch_e(const behavior<event<A, P>, P>& bea);
    template <class P EQ_DEF_PART, class T>
    behavior<typename T::time, P> clock(const T& t);

    namespace impl {

        class behavior_;
        class behavior_impl;

        class event_ {
        friend class behavior_;
        template <class A, class P> friend class sodium::event;
        template <class A, class P> friend class sodium::event_loop;
        template <class A, class P> friend class sodium::behavior;
        friend behavior_ switch_b(transaction_impl* trans, const behavior_& bba);
        friend SODIUM_SHARED_PTR<behavior_impl> hold(transaction_impl* trans0, const light_ptr& initValue, const event_& input);
        friend SODIUM_SHARED_PTR<behavior_impl> hold_lazy(transaction_impl* trans0, const std::function<light_ptr()>& initValue, const event_& input);
        template <class A, class B, class P>
#if defined(SODIUM_NO_CXX11)
        friend behavior<B, P> sodium::apply(const behavior<lambda1<B, const A&>, P>& bf, const behavior<A, P>& ba);
#else
        friend behavior<B, P> sodium::apply(const behavior<std::function<B(const A&)>, P>& bf, const behavior<A, P>& ba);
#endif
        friend behavior_ apply(transaction_impl* trans0, const behavior_& bf, const behavior_& ba);
#if defined(SODIUM_NO_CXX11)
        friend event_ map_(transaction_impl* trans, const lambda1<light_ptr, const light_ptr&>& f, const event_& ev);
#else
        friend event_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f, const event_& ev);
#endif
        friend behavior_ map_(transaction_impl* trans,
#if defined(SODIUM_NO_CXX11)
            const lambda1<light_ptr, const light_ptr&>& f,
#else
            const std::function<light_ptr(const light_ptr&)>& f,
#endif
            const behavior_& beh);
        friend event_ switch_e(transaction_impl* trans, const behavior_& bea);
        template <class A, class P>
        friend event<A, P> sodium::split(const event<std::list<A>, P>& e);
#if defined(SODIUM_NO_CXX11)
        friend struct switch_e_task;
        friend struct switch_b_handler;
#endif
        friend event_ filter_optional_(transaction_impl* trans, const event_& input,
            const std::function<boost::optional<light_ptr>(const light_ptr&)>& f);

        protected:
            boost::intrusive_ptr<listen_impl_func<H_EVENT> > p_listen_impl;

        public:
            event_();
            event_(const boost::intrusive_ptr<listen_impl_func<H_EVENT> >& p_listen_impl)
                : p_listen_impl(p_listen_impl) {}

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            bool is_never() const { return !impl::alive(p_listen_impl); }
#endif

        protected:

            /*!
             * listen to events.
             */
#if defined(SODIUM_NO_CXX11)
            lambda0<void>* listen_raw(
#else
            std::function<void()>* listen_raw(
#endif
                        transaction_impl* trans0,
                        const SODIUM_SHARED_PTR<impl::node>& target,
#if defined(SODIUM_NO_CXX11)
                        lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>* handle,
#else
                        std::function<void(const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&)>* handle,
#endif
                        bool suppressEarlierFirings) const;

            /*!
             * This is far more efficient than add_cleanup because it modifies the event
             * in place.
             */
#if defined(SODIUM_NO_CXX11)
            event_ unsafe_add_cleanup(lambda0<void>* cleanup)
#else
            event_ unsafe_add_cleanup(std::function<void()>* cleanup)
#endif
            {
                boost::intrusive_ptr<listen_impl_func<H_STRONG> > li(
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
#if defined(SODIUM_NO_CXX11)
            event_ unsafe_add_cleanup(lambda0<void>* cleanup1, lambda0<void>* cleanup2)
#else
            event_ unsafe_add_cleanup(std::function<void()>* cleanup1, std::function<void()>* cleanup2)
#endif
            {
                boost::intrusive_ptr<listen_impl_func<H_STRONG> > li(
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
             * This is far more efficient than add_cleanup because it modifies the event
             * in place.
             */
#if defined(SODIUM_NO_CXX11)
            event_ unsafe_add_cleanup(lambda0<void>* cleanup1, lambda0<void>* cleanup2, lambda0<void>* cleanup3)
#else
            event_ unsafe_add_cleanup(std::function<void()>* cleanup1, std::function<void()>* cleanup2, std::function<void()>* cleanup3)
#endif
            {
                boost::intrusive_ptr<listen_impl_func<H_STRONG> > li(
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
                if (cleanup3 != NULL) {
                    if (alive(li))
                        li->cleanups.push_front(cleanup3);
                    else {
                        (*cleanup3)();
                        delete cleanup3;
                    }
                }
                return *this;
            }

            /*!
             * Create a new event that is like this event but has an extra cleanup.
             */
#if defined(SODIUM_NO_CXX11)
            event_ add_cleanup_(transaction_impl* trans, lambda0<void>* cleanup) const;
#else
            event_ add_cleanup_(transaction_impl* trans, std::function<void()>* cleanup) const;
#endif
            behavior_ hold_(transaction_impl* trans, const light_ptr& initA) const;
            behavior_ hold_lazy_(transaction_impl* trans, const std::function<light_ptr()>& initA) const;
            event_ once_(transaction_impl* trans) const;
            event_ merge_(transaction_impl* trans, const event_& other) const;
#if defined(SODIUM_NO_CXX11)
            event_ coalesce_(transaction_impl* trans, const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine) const;
#else
            event_ coalesce_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const;
#endif
            event_ last_firing_only_(transaction_impl* trans) const;
#if defined(SODIUM_NO_CXX11)
            event_ snapshot_(transaction_impl* trans, const behavior_& beh, const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine) const;
            event_ filter_(transaction_impl* trans, const lambda1<bool, const light_ptr&>& pred) const;
#else
            event_ snapshot_(transaction_impl* trans, const behavior_& beh, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const;
            event_ filter_(transaction_impl* trans, const std::function<bool(const light_ptr&)>& pred) const;
#endif

#if defined(SODIUM_NO_CXX11)
            lambda0<void>* listen_impl(
#else
            std::function<void()>* listen_impl(
#endif
                transaction_impl* trans,
                const SODIUM_SHARED_PTR<impl::node>& target,
                SODIUM_SHARED_PTR<holder> h,
                bool suppressEarlierFirings) const
            {
                boost::intrusive_ptr<listen_impl_func<H_STRONG> > li(
                    reinterpret_cast<listen_impl_func<H_STRONG>*>(p_listen_impl.get()));
                if (alive(li))
                    return (*li->func)(trans, target, h, suppressEarlierFirings);
                else
                    return NULL;
            }
        };
#if defined(SODIUM_NO_CXX11)
        template <class A, class B>
        class _de_type : public i_lambda1<light_ptr, const light_ptr&>
        {
        private:
            lambda1<B,const A&> f;
        public:
            _de_type(const lambda1<B,const A&>& f) : f(f) {}
            virtual light_ptr operator () (const light_ptr& a) const
            {
                return light_ptr::create<B>(f(*a.cast_ptr<A>(NULL)));
            }
        };
        #define SODIUM_DETYPE_FUNCTION1(A,B,f) new sodium::impl::_de_type<A,B>(f)
        event_ map_(transaction_impl* trans, const lambda1<light_ptr, const light_ptr&>& f, const event_& ca);
#else
        #define SODIUM_DETYPE_FUNCTION1(A,B,f) \
                   [f] (const light_ptr& a) -> light_ptr { \
                        return light_ptr::create<B>(f(*a.cast_ptr<A>(NULL))); \
                   }
        event_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f, const event_& ca);
#endif

        /*!
         * Function to push a value into an event
         */
        void send(const SODIUM_SHARED_PTR<node>& n, transaction_impl* trans, const light_ptr& ptr);

        /*!
         * Creates an event, that values can be pushed into using impl::send(). 
         */
        SODIUM_TUPLE<
                event_,
                SODIUM_SHARED_PTR<node>
            > unsafe_new_event();

        struct behavior_impl {
            behavior_impl();
            behavior_impl(
                const event_& updates,
                const SODIUM_SHARED_PTR<behavior_impl>& parent);
            virtual ~behavior_impl();

            virtual const light_ptr& sample() const = 0;
            virtual const light_ptr& newValue() const = 0;

            event_ updates;  // Having this here allows references to behavior to keep the
                             // underlying event's cleanups alive, and provides access to the
                             // underlying event, for certain primitives.

#if defined(SODIUM_NO_CXX11)
            lambda0<void>* kill;
#else
            std::function<void()>* kill;
#endif
            SODIUM_SHARED_PTR<behavior_impl> parent;

#if defined(SODIUM_NO_CXX11)
            lambda3<lambda0<void>, transaction_impl*, const SODIUM_SHARED_PTR<node>&,
                             const lambda2<void, transaction_impl*, const light_ptr&>&> listen_value_raw() const;
#else
            std::function<std::function<void()>(transaction_impl*, const SODIUM_SHARED_PTR<node>&,
                             const std::function<void(transaction_impl*, const light_ptr&)>&)> listen_value_raw() const;
#endif
        };

        SODIUM_SHARED_PTR<behavior_impl> hold(transaction_impl* trans0,
                            const light_ptr& initValue,
                            const event_& input);
        SODIUM_SHARED_PTR<behavior_impl> hold_lazy(transaction_impl* trans0,
                            const std::function<light_ptr()>& initValue,
                            const event_& input);

        struct behavior_impl_constant : behavior_impl {
            behavior_impl_constant(const light_ptr& k) : k(k) {}
            light_ptr k;
            virtual const light_ptr& sample() const { return k; }
            virtual const light_ptr& newValue() const { return k; }
        };

        template <class state_t>
        struct behavior_impl_concrete : behavior_impl {
            behavior_impl_concrete(
                const event_& updates,
                const state_t& state,
                const SODIUM_SHARED_PTR<behavior_impl>& parent)
            : behavior_impl(updates, parent),
              state(state)
            {
            }
            state_t state;

            virtual const light_ptr& sample() const { return state.sample(); }
            virtual const light_ptr& newValue() const { return state.newValue(); }
        };

        struct behavior_impl_loop : behavior_impl {
            behavior_impl_loop(
                const event_& updates,
                const SODIUM_SHARED_PTR<SODIUM_SHARED_PTR<behavior_impl> >& pLooped,
                const SODIUM_SHARED_PTR<behavior_impl>& parent)
            : behavior_impl(updates, parent),
              pLooped(pLooped)
            {
            }
            SODIUM_SHARED_PTR<SODIUM_SHARED_PTR<behavior_impl> > pLooped;

            void assertLooped() const {
                if (!*pLooped)
                    throw std::runtime_error("behavior_loop sampled before it was looped");
            }

            virtual const light_ptr& sample() const { assertLooped(); return (*pLooped)->sample(); }
            virtual const light_ptr& newValue() const { assertLooped(); return (*pLooped)->newValue(); }
        };

        struct behavior_state {
            behavior_state(const light_ptr& initA) : current(initA) {}
            light_ptr current;
            boost::optional<light_ptr> update;
            const light_ptr& sample() const { return current; }
            const light_ptr& newValue() const { return update ? update.get() : current; }
            void finalize() {
                current = update.get();
                update = boost::optional<light_ptr>();
            }
        };

        struct behavior_state_lazy {
            behavior_state_lazy(const std::function<light_ptr()>& initA)
            : pInitA(new std::function<light_ptr()>(initA)) {}
            std::function<light_ptr()>* pInitA;
            boost::optional<light_ptr> current;
            boost::optional<light_ptr> update;
            const light_ptr& sample() const {
                if (!current) {
                    const_cast<behavior_state_lazy*>(this)->current = boost::optional<light_ptr>((*pInitA)());
                    delete pInitA;
                    const_cast<behavior_state_lazy*>(this)->pInitA = NULL;
                }
                return current.get();
            }
            const light_ptr& newValue() const { return update ? update.get() : sample(); }
            void finalize() {
                current = update;
                update = boost::optional<light_ptr>();
            }
        };

        class behavior_ {
            friend impl::event_ underlying_event(const behavior_& beh);
            public:
                behavior_();
                behavior_(behavior_impl* impl);
                behavior_(const SODIUM_SHARED_PTR<behavior_impl>& impl);
                behavior_(const light_ptr& a);
                SODIUM_SHARED_PTR<impl::behavior_impl> impl;

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                /*!
                 * For optimization, if this behavior is a constant, then return its value.
                 */
                boost::optional<light_ptr> get_constant_value() const;
#endif

                event_ value_(transaction_impl* trans) const;
                const event_& updates_() const { return impl->updates; }
        };

#if defined(SODIUM_NO_CXX11)
        behavior_ map_(transaction_impl* trans, const lambda1<light_ptr, const light_ptr&>& f,
            const behavior_& beh);
#else
        behavior_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f,
            const behavior_& beh);
#endif

        template <class S>
        struct collect_state {
            collect_state(const std::function<S()>& s_lazy) : s_lazy(s_lazy) {}
            std::function<S()> s_lazy;
        };

#if defined(SODIUM_NO_CXX11)
        template <class A, class S, class B>
        struct collect_handler {
            collect_handler(const SODIUM_SHARED_PTR<collect_state<S> >& pState,
                            const lambda2<SODIUM_TUPLE<B, S>, const A&, const S&>& f)
            : pState(pState), f(f) {}
            SODIUM_SHARED_PTR<collect_state<S> > pState;
            lambda2<SODIUM_TUPLE<B, S>, const A&, const S&> f;
            virtual void operator () (const SODIUM_SHARED_PTR<node>& target, transaction_impl* trans,
                                      const light_ptr& ptr) {
                SODIUM_TUPLE<B,S> outsSt = f(*ptr.cast_ptr<A>(NULL), pState->s);
                pState->s = SODIUM_TUPLE_GET<1>(outsSt);
                send(target, trans, light_ptr::create<B>(SODIUM_TUPLE_GET<0>(outsSt)));
            }
        };
#endif
    }  // end namespace impl

#if defined(SODIUM_NO_CXX11)
    template <class A, class B>
    struct fst_arg : i_lambda2<A,const A&, const B&>  {
        virtual A operator () (const A& a, const B&) const { return a; }
    };
    template <class A, class B>
    struct snd_arg : i_lambda2<B,const A&, const B&> {
        virtual B operator () (const A&, const B& b) const { return b; }
    };
#endif

    template <class A, class P>
    class event;

    /*!
     * A like an event, but it tracks the input event's current value and causes it
     * always to be output once at the beginning for each listener.
     */
    template <class A, class P EQ_DEF_PART>
    class behavior : protected impl::behavior_ {
        template <class AA, class PP> friend class event;
        template <class AA, class PP> friend class behavior;
        template <class AA, class PP> friend class behavior_loop;
        template <class AA, class BB, class PP>
#if defined(SODIUM_NO_CXX11)
        friend behavior<BB, PP> apply(const behavior<lambda1<BB, const AA&>, PP>& bf, const behavior<AA, PP>& ba);
#else
        friend behavior<BB, PP> apply(const behavior<std::function<BB(const AA&)>, PP>& bf, const behavior<AA, PP>& ba);
#endif
        template <class AA, class PP>
        friend behavior<AA, PP> switch_b(const behavior<behavior<AA, PP>, PP>& bba);
        template <class AA, class PP>
        friend event<AA, PP> switch_e(const behavior<event<AA, PP>, PP>& bea);
        template <class PP, class TT>
        friend behavior<typename TT::time,PP> clock(const TT& t);
        private:
            behavior(const SODIUM_SHARED_PTR<impl::behavior_impl>& impl)
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

            behavior(A&& a)
                : impl::behavior_(light_ptr::create<A>(std::move(a)))
            {
            }

            /*!
             * Sample the value of this behavior.
             */
            A sample() const {
                transaction<P> trans;
                return *impl->sample().template cast_ptr<A>(NULL);
            }

            std::function<A()> sample_lazy() const {
                const SODIUM_SHARED_PTR<impl::behavior_impl>& impl(this->impl);
                return [impl] () -> A {
                    transaction<P> trans;
                    return *impl->sample().template cast_ptr<A>(NULL);
                };
            }

            /*!
             * Returns a new behavior with the specified cleanup added to it, such that
             * it will be executed when no copies of the new behavior are referenced.
             */
#if defined(SODIUM_NO_CXX11)
            behavior<A, P> add_cleanup(const lambda0<void>& cleanup) const {
#else
            behavior<A, P> add_cleanup(const std::function<void()>& cleanup) const {
#endif
                transaction<P> trans;
                return updates().add_cleanup(cleanup).hold(sample());
            }

            /*!
             * Map a function over this behaviour to modify the output value.
             */
            template <class B>
#if defined(SODIUM_NO_CXX11)
            behavior<B, P> map(const lambda1<B, const A&>& f) const {
#else
            behavior<B, P> map(const std::function<B(const A&)>& f) const {
#endif
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
#if defined(SODIUM_NO_CXX11)
            behavior<B, P> map_(const lambda1<B, const A&>& f) const {
#else
            behavior<B, P> map_(const std::function<B(const A&)>& f) const {
#endif
                transaction<P> trans;
                return behavior<B, P>(impl::map_(trans.impl(), SODIUM_DETYPE_FUNCTION1(A,B,f), *this));
            }

            /*!
             * Returns an event giving the updates to a behavior. If this behavior was created
             * by a hold, then this gives you back an event equivalent to the one that was held.
             */
            event<A, P> updates() const {
                return event<A, P>(impl->updates);
            }

            /*!
             * Returns an event describing the value of a behavior, where there's an initial event
             * giving the current value.
             */
            event<A, P> value() const {
                transaction<P> trans;
                return event<A, P>(value_(trans.impl()));
            }

            /**
             * Transform a behavior with a generalized state loop (a mealy machine). The function
             * is passed the input and the old state and returns the new state and output value.
             */
            template <class S, class B>
            behavior<B, P> collect_lazy(
                const std::function<S()>& initS,
#if defined(SODIUM_NO_CXX11)
                const lambda2<SODIUM_TUPLE<B, S>, const A&, const S&>& f
#else
                const std::function<std::tuple<B, S>(const A&, const S&)>& f
#endif
            ) const
            {
                transaction<P> trans;
#if defined(SODIUM_NO_CXX11)
                event<A, P> ea = updates().coalesce(lambda2<A,A,A>(new snd_arg<A,A>));
#else
                auto ea = updates().coalesce([] (const A&, const A& snd) -> A { return snd; });
#endif
                std::function<A()> za_lazy = sample_lazy();
                std::function<SODIUM_TUPLE<B,S>()> zbs = [za_lazy, initS, f] () -> SODIUM_TUPLE<B,S> {
                    return f(za_lazy(), initS());
                };
                SODIUM_SHARED_PTR<impl::collect_state<S> > pState(new impl::collect_state<S>([zbs] () -> S {
                    return SODIUM_TUPLE_GET<1>(zbs());
                }));
                SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
#if defined(SODIUM_NO_CXX11)
                lambda0<void>* kill = updates().listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
                    new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&>(
                        new impl::collect_handler<A,S,B>(pState, f)
                    ), false);
#else
                auto kill = updates().listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
                    new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            SODIUM_TUPLE<B,S> outsSt = f(*ptr.cast_ptr<A>(NULL), pState->s_lazy());
                            const S& new_s = SODIUM_TUPLE_GET<1>(outsSt);
                            pState->s_lazy = [new_s] () { return new_s; };
                            send(target, trans, light_ptr::create<B>(SODIUM_TUPLE_GET<0>(outsSt)));
                        }), false);
#endif
                return event<B, P>(SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill)).hold_lazy([zbs] () -> B {
                    return SODIUM_TUPLE_GET<0>(zbs());
                });
            }

            /**
             * Transform a behavior with a generalized state loop (a mealy machine). The function
             * is passed the input and the old state and returns the new state and output value.
             */
            template <class S, class B>
            behavior<B, P> collect(
                const S& initS,
#if defined(SODIUM_NO_CXX11)
                const lambda2<SODIUM_TUPLE<B, S>, const A&, const S&>& f
#else
                const std::function<std::tuple<B, S>(const A&, const S&)>& f
#endif
            ) const
            {
                return collect_lazy<S, B>([initS] () -> S { return initS; }, f);
            }

    };  // end class behavior

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        template <class A>
        struct listen_wrap : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&> {
            listen_wrap(const lambda1<void, const A&>& handle) : handle(handle) {}
            lambda1<void, const A&> handle;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl* trans, const light_ptr& ptr) const {
                handle(*ptr.cast_ptr<A>(NULL));
            }
        };
        struct null_action : i_lambda0<void> {
            virtual void operator () () const {}
        };
        template <class A>
        struct detype_combine : i_lambda2<light_ptr, const light_ptr&, const light_ptr&> {
            detype_combine(const lambda2<A, const A&, const A&>& combine) : combine(combine) {}
            lambda2<A, const A&, const A&> combine;
            virtual light_ptr operator () (const light_ptr& a, const light_ptr& b) const {
                return light_ptr::create<A>(combine(*a.cast_ptr<A>(NULL), *b.cast_ptr<A>(NULL)));
            }
        };
        template <class A>
        struct detype_pred : i_lambda1<bool, const light_ptr&> {
            detype_pred(const lambda1<bool, A>& pred) : pred(pred) {}
            lambda1<bool, A> pred;
            virtual bool operator () (const light_ptr& a) const {
                return pred(*a.cast_ptr<A>(NULL));
            }
        };
        template <class A, class B, class C>
        struct detype_snapshot : i_lambda2<light_ptr, const light_ptr&, const light_ptr&> {
            detype_snapshot(const lambda2<C, const A&, const B&>& combine) : combine(combine) {}
            lambda2<C, const A&, const B&> combine;
            virtual light_ptr operator () (const light_ptr& a, const light_ptr& b) const {
                return light_ptr::create<C>(combine(*a.cast_ptr<A>(NULL), *b.cast_ptr<B>(NULL)));
            }
        };
        template <class A>
        struct gate_handler : i_lambda2<boost::optional<A>, const A&, const bool&> {
            virtual boost::optional<A> operator () (const A& a, const bool& gated) {
                return gated ? boost::optional<A>(a) : boost::optional<A>();
            }
        };
        template <class A, class B>
        struct accum_handler : i_lambda3<void, const SODIUM_SHARED_PTR<node>&, transaction_impl*, const light_ptr&> {
            accum_handler(
                const SODIUM_SHARED_PTR<collect_state<B> >& pState,
                const lambda2<B, const A&, const B&>& f)
            : pState(pState), f(f) {}
            SODIUM_SHARED_PTR<collect_state<B> > pState;
            lambda2<B, const A&, const B&> f;

            virtual void operator () (const SODIUM_SHARED_PTR<node>& target, transaction_impl* trans, const light_ptr& ptr) const {
                pState->s = f(*ptr.cast_ptr<A>(NULL), pState->s);
                send(target, trans, light_ptr::create<B>(pState->s));
            }
        };
        template <class A>
        struct count_handler : i_lambda2<int,const A&,int> {
            virtual int operator () (const A&, int total) const {
                return total+1;
            }
        };
        template <class A>
        struct delay_handler : i_lambda1<std::list<A>, const A&> {
            virtual std::list<A> operator () (const A& a) const {
                std::list<A> as;
                as.push_back(a);
                return as;
            }
        };
    }
#endif

    template <class A, class P EQ_DEF_PART>
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
            event(const impl::event_& ev) : impl::event_(ev) {}
        public:
            /*!
             * High-level interface to obtain an event's value.
             */
#if defined(SODIUM_NO_CXX11)
            lambda0<void> listen(const lambda1<void, const A&>& handle) const {
#else
            std::function<void()> listen(const std::function<void(const A&)>& handle) const {
#endif
                transaction<P> trans;
#if defined(SODIUM_NO_CXX11)
                lambda0<void>* pKill = listen_raw(trans.impl(),
#else
                std::function<void()>* pKill = listen_raw(trans.impl(),
#endif
                    SODIUM_SHARED_PTR<impl::node>(new impl::node(SODIUM_IMPL_RANK_T_MAX)),
#if defined(SODIUM_NO_CXX11)
                    new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&>(
                        new impl::listen_wrap<A>(handle)
                    ), false);
#else
                    new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [handle] (const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl* trans, const light_ptr& ptr) {
                            handle(*ptr.cast_ptr<A>(NULL));
                        }), false);
#endif
                if (pKill != NULL) {
#if defined(SODIUM_NO_CXX11)
                    lambda0<void> kill(*pKill);
#else
                    std::function<void()> kill(*pKill);
#endif
                    delete pKill;
                    return kill;
                }
                else
#if defined(SODIUM_NO_CXX11)
                    return new impl::null_action();
#else
                    return [] () {};
#endif
            };

            /*!
             * Map a function over this event to modify the output value. The function must be
             * pure (referentially transparent), that is, it must not have effects.
             */
            template <class B>
#if defined(SODIUM_NO_CXX11)
            event<B, P> map(const lambda1<B, const A&>& f) const {
#else
            event<B, P> map(const std::function<B(const A&)>& f) const {
#endif
                transaction<P> trans;
                return event<B, P>(impl::map_(trans.impl(), SODIUM_DETYPE_FUNCTION1(A,B,f), *this));
            }

            /*!
             * Map a function over this event to modify the output value. Effects are allowed.
             */
            template <class B>
#if defined(SODIUM_NO_CXX11)
            event<B, P> map_effectful(const lambda1<B, const A&>& f) const {
#else
            event<B, P> map_effectful(const std::function<B(const A&)>& f) const {
#endif
                return this->template map_<B>(f);  // Same as map() for now but this may change!
            }

            /*!
             * Map a function over this event to modify the output value.
             *
             * g++-4.7.2 has a bug where, under a 'using namespace std' it will interpret
             * b.template map<A>(f) as if it were std::map. If you get this problem, you can
             * work around it with map_.
             */
            template <class B>
#if defined(SODIUM_NO_CXX11)
            event<B, P> map_(const lambda1<B, const A&>& f) const {
#else
            event<B, P> map_(const std::function<B(const A&)>& f) const {
#endif
                transaction<P> trans;
                return event<B, P>(impl::map_(trans.impl(), SODIUM_DETYPE_FUNCTION1(A,B,f), *this));
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
#if defined(SODIUM_NO_CXX11)
            event<A, P> coalesce(const lambda2<A, const A&, const A&>& combine) const
#else
            event<A, P> coalesce(const std::function<A(const A&, const A&)>& combine) const
#endif
            {
                transaction<P> trans;
                return event<A, P>(coalesce_(trans.impl(),
#if defined(SODIUM_NO_CXX11)
                    new impl::detype_combine<A>(combine)
#else
                    [combine] (const light_ptr& a, const light_ptr& b) -> light_ptr {
                        return light_ptr::create<A>(combine(*a.cast_ptr<A>(NULL), *b.cast_ptr<A>(NULL)));
                    }
#endif
                ));
            }

            /*!
             * If there's more than one firing in a single transaction, keep only the latest one.
             */
            event<A, P> coalesce() const
            {
                return coalesce(
#if defined(SODIUM_NO_CXX11)
                        new snd_arg<A,A>
#else
                        [] (const A&, const A& snd) -> A { return snd; }
#endif
                    );
            }

            /*!
             * Merge two streams of events of the same type, combining simultaneous
             * event occurrences.
             *
             * In the case where multiple event occurrences are simultaneous (i.e. all
             * within the same transaction), they are combined using the same logic as
             * 'coalesce'.
             */
#if defined(SODIUM_NO_CXX11)
            event<A, P> merge(const event<A, P>& other, const lambda2<A, const A&, const A&>& combine) const
#else
            event<A, P> merge(const event<A, P>& other, const std::function<A(const A&, const A&)>& combine) const
#endif
            {
                return merge(other).coalesce(combine);
            }

            /*!
             * Filter this event based on the specified predicate, passing through values
             * where the predicate returns true.
             */
#if defined(SODIUM_NO_CXX11)
            event<A, P> filter(const lambda1<bool, const A&>& pred) const
#else
            event<A, P> filter(const std::function<bool(const A&)>& pred) const
#endif
            {
                transaction<P> trans;
                return event<A, P>(filter_(trans.impl(),
#if defined(SODIUM_NO_CXX11)
                    new impl::detype_pred<A>(pred)
#else
                    [pred] (const light_ptr& a) {
                        return pred(*a.cast_ptr<A>(NULL));
                    }
#endif
                  ));
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

            behavior<A, P> hold(A&& initA) const
            {
                transaction<P> trans;
                return behavior<A, P>(hold_(trans.impl(), light_ptr::create<A>(std::move(initA))));
            }

            behavior<A, P> hold_lazy(const std::function<A()>& initA) const
            {
                transaction<P> trans;
                return behavior<A, P>(hold_lazy_(trans.impl(), [initA] () -> light_ptr { return light_ptr::create<A>(initA()); }));
            }

            /*!
             * Sample the behavior's value as at the transaction before the
             * current one, i.e. no changes from the current transaction are
             * taken.
             */
            template <class B, class C>
#if defined(SODIUM_NO_CXX11)
            event<C, P> snapshot(const behavior<B, P>& beh, const lambda2<C, const A&, const B&>& combine) const
#else
            event<C, P> snapshot(const behavior<B, P>& beh, const std::function<C(const A&, const B&)>& combine) const
#endif
            {
                transaction<P> trans;
                return event<C, P>(snapshot_(trans.impl(), beh,
#if defined(SODIUM_NO_CXX11)
                    new impl::detype_snapshot<A,B,C>(combine)
#else
                    [combine] (const light_ptr& a, const light_ptr& b) -> light_ptr {
                        return light_ptr::create<C>(combine(*a.cast_ptr<A>(NULL), *b.cast_ptr<B>(NULL)));
                    }
#endif
                ));
            }

            /*!
             * Sample the behavior's value as at the transaction before the
             * current one, i.e. no changes from the current transaction are
             * taken.
             */
            template <class B>
            event<B, P> snapshot(const behavior<B, P>& beh) const
            {
                return snapshot<B, B>(beh,
#if defined(SODIUM_NO_CXX11)
                    new snd_arg<A,B>
#else
                    [] (const A&, const B& b) { return b; }
#endif
                    );
            }

            /*!
             * Allow events through only when the behavior's value is true.
             */
            event<A, P> gate(const behavior<bool, P>& g) const
            {
                transaction<P> trans;
                return filter_optional<A, P>(snapshot<bool, boost::optional<A>>(
                    g,
#if defined(SODIUM_NO_CXX11)
                    new impl::gate_handler<A>()
#else
                    [] (const A& a, const bool& gated) {
                        return gated ? boost::optional<A>(a) : boost::optional<A>();
                    }
#endif
                ));
            }

            /*!
             * Adapt an event to a new event statefully.  Always outputs one output for each
             * input.
             */
            template <class S, class B>
            event<B, P> collect_lazy(
                const std::function<S()>& initS,
#if defined(SODIUM_NO_CXX11)
                const lambda2<SODIUM_TUPLE<B, S>, const A&, const S&>& f
#else
                const std::function<SODIUM_TUPLE<B, S>(const A&, const S&)>& f
#endif
            ) const
            {
                transaction<P> trans;
                SODIUM_SHARED_PTR<impl::collect_state<S> > pState(new impl::collect_state<S>(initS));
                SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
#if defined(SODIUM_NO_CXX11)
                lambda0<void>* kill = listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
                    new impl::collect_handler<A,S,B>(pState, f), false);
#else
                auto kill = listen_raw(trans.impl(), std::get<1>(p),
                    new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            auto outsSt = f(*ptr.cast_ptr<A>(NULL), pState->s_lazy());
                            const S& new_s = SODIUM_TUPLE_GET<1>(outsSt);
                            pState->s_lazy = [new_s] () { return new_s; };
                            send(target, trans, light_ptr::create<B>(std::get<0>(outsSt)));
                        }), false);
#endif
                return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill);
            }

            /*!
             * Adapt an event to a new event statefully.  Always outputs one output for each
             * input.
             */
            template <class S, class B>
            event<B, P> collect(
                const S& initS,
#if defined(SODIUM_NO_CXX11)
                const lambda2<SODIUM_TUPLE<B, S>, const A&, const S&>& f
#else
                const std::function<SODIUM_TUPLE<B, S>(const A&, const S&)>& f
#endif
            ) const
            {
                return collect_lazy<S,B>([initS] () -> S { return initS; }, f);
            }

            template <class B>
            event<B, P> accum_e_lazy(
                const std::function<B()>& initB,
#if defined(SODIUM_NO_CXX11)
                const lambda2<B, const A&, const B&>& f
#else
                const std::function<B(const A&, const B&)>& f
#endif
            ) const
            {
                transaction<P> trans;
                SODIUM_SHARED_PTR<impl::collect_state<B> > pState(new impl::collect_state<B>(initB));
                SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
#if defined(SODIUM_NO_CXX11)
                lambda0<void>* kill = listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
                    new impl::accum_handler<A,B>(pState, f)
#else
                auto kill = listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
                    new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            B b = f(*ptr.cast_ptr<A>(NULL), pState->s_lazy());
                            pState->s_lazy = [b] () { return b; };
                            send(target, trans, light_ptr::create<B>(b));
                        })
#endif
                    , false);
                return event<B, P>(SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill));
            }

            template <class B>
            event<B, P> accum_e(
                const B& initB,
#if defined(SODIUM_NO_CXX11)
                const lambda2<B, const A&, const B&>& f
#else
                const std::function<B(const A&, const B&)>& f
#endif
            ) const
            {
                return accum_e_lazy<B>([initB] () -> B { return initB; }, f);
            }

            template <class B>
            behavior<B, P> accum(
                const B& initB,
#if defined(SODIUM_NO_CXX11)
                const lambda2<B, const A&, const B&>& f
#else
                const std::function<B(const A&, const B&)>& f
#endif
            ) const
            {
                return accum_e(initB, f).hold(initB);
            }

            behavior<int, P> count() const
            {
                return accum<int>(0,
#if defined(SODIUM_NO_CXX11)
                    new impl::count_handler<A>
#else
                    [] (const A&, const int& total) -> int {  return total+1; }
#endif
                );
            }

            event<A, P> once() const
            {
                transaction<P> trans;
                return event<A, P>(once_(trans.impl()));
            }

            /*!
             * Delays each event occurrence by putting it into a new transaction, using
             * the same method as split.
             */
            event<A, P> delay()
            {
                return split<A,P>(map_<std::list<A> >(
#if defined(SODIUM_NO_CXX11)
                        new impl::delay_handler<A>
#else
                        [] (const A& a) -> std::list<A> { return { a }; }
#endif
                    ));
            }

            /*!
             * Add a clean-up operation to be performed when this event is no longer
             * referenced.
             */
#if defined(SODIUM_NO_CXX11)
            event<A, P> add_cleanup(const lambda0<void>& cleanup) const
#else
            event<A, P> add_cleanup(const std::function<void()>& cleanup) const
#endif
            {
                transaction<P> trans;
                return event<A, P>(add_cleanup_(trans.impl(),
#if defined(SODIUM_NO_CXX11)
                    new lambda0<void>(cleanup)
#else
                    new std::function<void()>(cleanup)
#endif
                ));
            }
    };  // end class event

    namespace impl {
        struct event_sink_impl {
            event_sink_impl();
            event_ construct();
            void send(transaction_impl* trans, const light_ptr& ptr) const;
            SODIUM_SHARED_PTR<impl::node> target;
        };
    }

    /*!
     * An event with a send() method to allow values to be pushed into it
     * from the imperative world.
     */
    template <class A, class P EQ_DEF_PART>
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

            void send(A&& a) const {
                transaction<P> trans;
                impl.send(trans.impl(), light_ptr::create<A>(std::move(a)));
            }
    };

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        template <class A, class Q>
        struct cross_post : i_lambda0<void> {
            cross_post(const event_sink<A, Q>& s, const A& a) : s(s), a(a) {}
            event_sink<A, Q> s;
            A a;
            void operator () () const {
                s.send(a);
            }
        };
        template <class A, class P, class Q>
        struct cross_handler : i_lambda1<void, const A&> {
            cross_handler(const event_sink<A, Q>& s) : s(s) {}
            event_sink<A, Q> s;
            virtual void operator () (const A& a) const {
                transaction<P> trans;
                trans.impl()->part->post(new cross_post<A,Q>(s, a));
            }
        };
    }
#endif

    /*!
     * Make the specified event cross to partition Q.
     */
    template <class A, class P, class Q>
    event<A, Q> cross(const event<A, P>& e)
    {
        transaction<P> trans;
        event_sink<A, Q> s;
#if defined(SODIUM_NO_CXX11)
        lambda0<void> kill = e.listen(new impl::cross_handler<A, P, Q>(s));
#else
        auto kill = e.listen([s] (const A& a) {
            transaction<P> trans;
            trans.impl()->part->post([s, a] () {
                s.send(a);
            });
        });
#endif
        return s.add_cleanup(kill);
    }

    /*!
     * Make the specified behavior cross to partition Q.
     */
    template <class A, class P, class Q>
    behavior<A, Q> cross(const behavior<A, P>& b)
    {
        transaction<P> trans;
        return cross<A, P, Q>(b.updates()).hold(b.sample());
    }

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        template <class A>
        struct filter_optional_handler : public i_lambda3<void,const SODIUM_SHARED_PTR<node>&, transaction_impl*, const light_ptr&> {
            virtual void operator () (const SODIUM_SHARED_PTR<node>& target, transaction_impl* trans, const light_ptr& poa) const {
                const boost::optional<A>& oa = *poa.cast_ptr<boost::optional<A> >(NULL);
                if (oa) send(target, trans, light_ptr::create<A>(oa.get()));
            }
        };
    }
#endif

    namespace impl {
        event_ filter_optional_(transaction_impl* trans, const event_& input,
            const std::function<boost::optional<light_ptr>(const light_ptr&)>& f);
    }

    /*!
     * Filter an event of optionals, keeping only the defined values.
     */
    template <class A, class P>
    event<A, P> filter_optional(const event<boost::optional<A>, P>& input)
    {
        transaction<P> trans;
        return impl::filter_optional_(trans.impl(), input, [] (const light_ptr& poa) -> boost::optional<light_ptr> {
            const boost::optional<A>& oa = *poa.cast_ptr<boost::optional<A>>(NULL);
            if (oa)
                return boost::optional<light_ptr>(light_ptr::create<A>(oa.get()));
            else
                return boost::optional<light_ptr>();
        });
    }

    /*!
     * A behavior with a send() method to allow its value to be changed
     * from the imperative world.
     */
    template <class A, class P EQ_DEF_PART>
    class behavior_sink : public behavior<A, P>
    {
        private:
            event_sink<A, P> e;

            behavior_sink(const behavior<A, P>& beh) : behavior<A, P>(beh) {}

        public:
            behavior_sink(const A& initA)
            {
                transaction<P> trans;
                this->impl = SODIUM_SHARED_PTR<impl::behavior_impl>(hold(trans.impl(), light_ptr::create<A>(initA), e));
            }

            behavior_sink(A&& initA)
            {
                transaction<P> trans;
                this->impl = SODIUM_SHARED_PTR<impl::behavior_impl>(hold(trans.impl(), light_ptr::create<A>(std::move(initA)), e));
            }

            void send(const A& a) const
            {
                e.send(a);
            }

            void send(A&& a) const
            {
                e.send(std::move(a));
            }
    };

    namespace impl {
        /*!
         * Returns an event describing the changes in a behavior.
         */
        inline impl::event_ underlying_event(const impl::behavior_& beh) {return beh.impl->updates;}
    };

    namespace impl {
        behavior_ apply(transaction_impl* trans, const behavior_& bf, const behavior_& ba);
        
#if defined(SODIUM_NO_CXX11)
        template <class A, class B>
        struct apply_handler : i_lambda1<light_ptr, const light_ptr&> {
            virtual light_ptr operator () (const light_ptr& pf) const {
                const lambda1<B, const A&>& f = *pf.cast_ptr<lambda1<B, const A&> >(NULL);
                return light_ptr::create<lambda1<light_ptr, const light_ptr&> >(
                        SODIUM_DETYPE_FUNCTION1(A, B, f)
                    );
            }
        };
#endif
    };

    /*!
     * Apply a function contained in a behavior to a behavior value. This is the primitive
     * for all lifting of functions into behaviors.
     */
    template <class A, class B, class P>
    behavior<B, P> apply(
#if defined(SODIUM_NO_CXX11)
        const behavior<lambda1<B, const A&>, P>& bf,
#else
        const behavior<std::function<B(const A&)>, P>& bf,
#endif
        const behavior<A, P>& ba)
    {
        transaction<P> trans;
        return behavior<B, P>(impl::apply(
            trans.impl(),
            impl::map_(trans.impl(),
#if defined(SODIUM_NO_CXX11)
                new impl::apply_handler<A,B>,
#else
                [] (const light_ptr& pf) -> light_ptr {
                    const std::function<B(const A&)>& f = *pf.cast_ptr<std::function<B(const A&)>>(NULL);
                    return light_ptr::create<std::function<light_ptr(const light_ptr&)> >(
                            SODIUM_DETYPE_FUNCTION1(A, B, f)
                        );
                },
#endif
                bf),
            ba
        ));
    }

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        struct event_non_looped_kill : i_lambda0<void> {
            virtual void operator () () const {
            }
        };
        struct event_loop_kill : i_lambda0<void> {
            event_loop_kill(const SODIUM_SHARED_PTR<lambda0<void>*>& pKill) : pKill(pKill) {}
            SODIUM_SHARED_PTR<lambda0<void>*> pKill;
            virtual void operator () () const {
                lambda0<void>* kill = *pKill;
                if (kill)
                    (*kill)();
                delete kill;
            }
        };
    }
#endif

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
    template <class A, class P EQ_DEF_PART>
    class event_loop : public event<A, P>
    {
        private:
            struct info {
                info(
#if defined(SODIUM_NO_CXX11)
                    const SODIUM_SHARED_PTR<lambda0<void>*>& pKill
#else
                    const SODIUM_SHARED_PTR<std::function<void()>*>& pKill
#endif
                )
                : pKill(pKill), looped(false)
                {
                }
                SODIUM_SHARED_PTR<impl::node> target;
#if defined(SODIUM_NO_CXX11)
                SODIUM_SHARED_PTR<lambda0<void>*> pKill;
#else
                SODIUM_SHARED_PTR<std::function<void()>*> pKill;
#endif
                bool looped;
            };
            SODIUM_SHARED_PTR<info> i;

        private:
            event_loop(const impl::event_& ev, const SODIUM_SHARED_PTR<info>& i) : event<A, P>(ev), i(i) {}

        public:
            event_loop()
            {
#if defined(SODIUM_NO_CXX11)
                SODIUM_SHARED_PTR<lambda0<void>*> pKill(
                    new lambda0<void>*(new lambda0<void>(new impl::event_non_looped_kill))
                );
#else
                SODIUM_SHARED_PTR<std::function<void()>*> pKill(
                    new std::function<void()>*(new std::function<void()>(
                        [] () {
                        }
                    ))
                );
#endif
                SODIUM_SHARED_PTR<info> i(new info(pKill));

                SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
                i->target = SODIUM_TUPLE_GET<1>(p);
                *this = event_loop<A, P>(
                    SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(
#if defined(SODIUM_NO_CXX11)
                        new lambda0<void>(new impl::event_loop_kill(pKill))
#else
                        new std::function<void()>(
                            [pKill] () {
                                std::function<void()>* kill = *pKill;
                                if (kill)
                                    (*kill)();
                                delete kill;
                            }
                        )
#endif
                    ),
                    i
                );
            }

            void loop(const event<A, P>& e)
            {
                if (!i->looped) {
                    transaction<P> trans;
                    SODIUM_SHARED_PTR<impl::node> target(i->target);
                    *i->pKill = e.listen_raw(trans.impl(), target, NULL, false);
                    i->looped = true;
                }
                else {
#if defined(SODIUM_NO_EXCEPTIONS)
                    abort();
#else
                    throw std::runtime_error("event_loop looped back more than once");
#endif
                }
            }
    };

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        struct behavior_non_looped_sample : i_lambda0<light_ptr> {
            virtual light_ptr operator () () const {
#if defined(SODIUM_NO_EXCEPTIONS)
                abort();
                return light_ptr();
#else
                throw std::runtime_error("behavior_loop sampled before it was looped");
#endif
            }
        };
    }
#endif

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
    template <class A, class P EQ_DEF_PART>
    class behavior_loop : public behavior<A, P>
    {
        private:
            event_loop<A, P> elp;
            SODIUM_SHARED_PTR<SODIUM_SHARED_PTR<impl::behavior_impl> > pLooped;

        public:
            behavior_loop()
                : behavior<A, P>(impl::behavior_()),
                  pLooped(new SODIUM_SHARED_PTR<impl::behavior_impl>)
            {
                this->impl = SODIUM_SHARED_PTR<impl::behavior_impl>(new impl::behavior_impl_loop(
                    elp,
                    pLooped,
                    SODIUM_SHARED_PTR<impl::behavior_impl>()));
            }

            void loop(const behavior<A, P>& b)
            {
                elp.loop(b.updates());
                *pLooped = b.impl;
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
    template <class A, class P>
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
    template <class A, class P EQ_DEF_PART>
    behavior<A, P> switch_b(const behavior<behavior<A, P>, P>& bba)
    {
        transaction<P> trans;
        return behavior<A, P>(impl::switch_b(trans.impl(), bba));
    }

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        template <class A, class B, class C>
        struct lift2_handler2 : i_lambda1<C, const B&> {
            lift2_handler2(const lambda2<C, const A&, const B&>& f, const A& a) : f(f), a(a) {}
            lambda2<C, const A&, const B&> f;
            A a;
            virtual C operator () (const B& b) const {
                return f(a, b);
            }
        };
        template <class A, class B, class C>
        struct lift2_handler1 : i_lambda1<lambda1<C, const B&>, const A&> {
            lift2_handler1(const lambda2<C, const A&, const B&>& f) : f(f) {}
            lambda2<C, const A&, const B&> f;
            virtual lambda1<C, const B&> operator () (const A& a) const {
                return new lift2_handler2<A, B, C>(f, a);
            }
        };
    }
#endif

    /*!
     * Lift a binary function into behaviors.
     */
    template <class A, class B, class C, class P EQ_DEF_PART>
#if defined(SODIUM_NO_CXX11)
    behavior<C, P> lift(const lambda2<C, const A&, const B&>& f, const behavior<A, P>& ba, const behavior<B, P>& bb)
#else
    behavior<C, P> lift(const std::function<C(const A&, const B&)>& f, const behavior<A, P>& ba, const behavior<B, P>& bb)
#endif
    {
#if defined(SODIUM_NO_CXX11)
        lambda1<lambda1<C, const B&>, const A&> fa(
            new impl::lift2_handler1<A,B,C>(f)
        );
#else
        std::function<std::function<C(const B&)>(const A&)> fa(
            [f] (const A& a) -> std::function<C(const B&)> {
                return [f, a] (const B& b) -> C { return f(a, b); };
            }
        );
#endif
        transaction<P> trans;
        return apply<B, C>(ba.map_(fa), bb);
    }

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        template <class A, class B, class C, class D>
        struct lift3_handler3 : i_lambda1<C, const B&> {
            lift3_handler3(const lambda3<D, const A&, const B&, const C&>& f, const A& a, const B& b)
                : f(f), a(a), b(b) {}
            lambda3<D, const A&, const B&, const C&> f;
            A a;
            B b;
            virtual D operator () (const C& c) const {
                return f(a, b, c);
            }
        };
        template <class A, class B, class C, class D>
        struct lift3_handler2 : i_lambda1<lambda1<D, const C&>, const B&> {
            lift3_handler2(const lambda3<D, const A&, const B&, const C&>& f, const A& a) : f(f), a(a) {}
            lambda3<D, const A&, const B&, const C&> f;
            A a;
            virtual lambda1<D, const C&> operator () (const B& b) const {
                return new lift3_handler3<A, B, C, D>(f, a, b);
            }
        };
        template <class A, class B, class C, class D>
        struct lift3_handler1 : i_lambda1<lambda1<lambda1<D, const C&>, const B&>, const A&> {
            lift3_handler1(const lambda3<D, const A&, const B&, const C&>& f) : f(f) {}
            lambda3<D, const A&, const B&, const C&> f;
            virtual lambda1<lambda1<D, const C&>, const B&> operator () (const A& a) const {
                return new lift3_handler2<A, B, C, D>(f, a);
            }
        };
    }
#endif

    /*!
     * Lift a ternary function into behaviors.
     */
    template <class A, class B, class C, class D, class P EQ_DEF_PART>
#if defined(SODIUM_NO_CXX11)
    behavior<D, P> lift(const lambda3<D, const A&, const B&, const C&>& f,
#else
    behavior<D, P> lift(const std::function<D(const A&, const B&, const C&)>& f,
#endif
        const behavior<A, P>& ba,
        const behavior<B, P>& bb,
        const behavior<C, P>& bc
    )
    {
#if defined(SODIUM_NO_CXX11)
        lambda1<lambda1<lambda1<D, const C&>, const B&>, const A&> fa(
            new impl::lift3_handler1<A, B, C, D>(f)
        );
#else
        std::function<std::function<std::function<D(const C&)>(const B&)>(const A&)> fa(
            [f] (const A& a) -> std::function<std::function<D(const C&)>(const B&)> {
                return [f, a] (const B& b) -> std::function<D(const C&)> {
                    return [f, a, b] (const C& c) -> D {
                        return f(a,b,c);
                    };
                };
            }
        );
#endif
        return apply(apply(ba.map_(fa), bb), bc);
    }

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        template <class A, class B, class C, class D, class E>
        struct lift4_handler4 : i_lambda1<E, const D&> {
            lift4_handler4(const lambda4<E, const A&, const B&, const C&, const D&>& f, const A& a, const B& b, const C& c)
                : f(f), a(a), b(b), c(c) {}
            lambda4<E, const A&, const B&, const C&, const D&> f;
            A a;
            B b;
            C c;
            virtual E operator () (const D& d) const {
                return f(a, b, c, d);
            }
        };
        template <class A, class B, class C, class D, class E>
        struct lift4_handler3 : i_lambda1<lambda1<E, const D&>, const C&> {
            lift4_handler3(const lambda4<E, const A&, const B&, const C&, const D&>& f, const A& a, const B& b)
                : f(f), a(a), b(b) {}
            lambda4<E, const A&, const B&, const C&, const D&> f;
            A a;
            B b;
            virtual E operator () (const C& c) const {
                return new lift4_handler4<A, B, C, D, E>(f, a, b, c);
            }
        };
        template <class A, class B, class C, class D, class E>
        struct lift4_handler2 : i_lambda1<lambda1<lambda1<E, const D&>, const C&>, const B&> {
            lift4_handler2(const lambda4<E, const A&, const B&, const C&, const D&>& f, const A& a) : f(f), a(a) {}
            lambda4<E, const A&, const B&, const C&, const D&> f;
            A a;
            virtual lambda1<D, const C&> operator () (const B& b) const {
                return new lift4_handler3<A, B, C, D, E>(f, a, b);
            }
        };
        template <class A, class B, class C, class D, class E>
        struct lift4_handler1 : i_lambda1<lambda1<lambda1<lambda1<E, const D&>, const C&>, const B&>, const A&> {
            lift4_handler1(const lambda4<E, const A&, const B&, const C&, const D&>& f) : f(f) {}
            lambda4<E, const A&, const B&, const C&, const D&> f;
            virtual lambda1<lambda1<D, const C&>, const B&> operator () (const A& a) const {
                return new lift4_handler2<A, B, C, D, E>(f, a);
            }
        };
    }
#endif

    /*!
     * Lift a quaternary function into behaviors.
     */
    template <class A, class B, class C, class D, class E, class P EQ_DEF_PART>
#if defined(SODIUM_NO_CXX11)
    behavior<E, P> lift(const lambda4<E, const A&, const B&, const C&, const D&>& f,
#else
    behavior<E, P> lift(const std::function<E(const A&, const B&, const C&, const D&)>& f,
#endif
        const behavior<A, P>& ba,
        const behavior<B, P>& bb,
        const behavior<C, P>& bc,
        const behavior<D, P>& bd
    )
    {
#if defined(SODIUM_NO_CXX11)
        lambda1<lambda1<lambda1<lambda1<E, const D&>, const C&>, const B&>, const A&> fa(
            new impl::lift4_handler1<A,B,C,D,E>(f)
        );
#else
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
#endif
        return apply(apply(apply(ba.map_(fa), bb), bc), bd);
    }

    /*!
     * Lift a 5-argument function into behaviors.
     */
    template <class A, class B, class C, class D, class E, class F, class P EQ_DEF_PART>
#if defined(SODIUM_NO_CXX11)
    behavior<E, P> lift(const lambda5<E, const A&, const B&, const C&, const D&, const E&>& f,
#else
    behavior<E, P> lift(const std::function<F(const A&, const B&, const C&, const D&, const E&)>& f,
#endif
        const behavior<A, P>& ba,
        const behavior<B, P>& bb,
        const behavior<C, P>& bc,
        const behavior<D, P>& bd,
        const behavior<E, P>& be
    )
    {
#if defined(SODIUM_NO_CXX11)
        lambda1<lambda1<lambda1<lambda1<lambda1<lambda1<F, const E&>, const D&>, const C&>, const B&>, const A&>> fa(
            new impl::lift5_handler1<A,B,C,D,E,F>(f)
        );
#else
        std::function<std::function<std::function<std::function<std::function<F(const E&)>(const D&)>(const C&)>(const B&)>(const A&)> fa(
            [f] (const A& a) -> std::function<std::function<std::function<std::function<F(const E&)>(const D&)>(const C&)>(const B&)> {
                return [f, a] (const B& b) -> std::function<std::function<std::function<F(const E&)>(const D&)>(const C&)> {
                    return [f, a, b] (const C& c) -> std::function<std::function<F(const E&)>(const D&)> {
                        return [f, a, b, c] (const D& d) -> std::function<F(const E&)> {
                            return [f, a, b, c, d] (const E& e) -> F {
                                return f(a,b,c,d,e);
                            };
                        };
                    };
                };
            }
        );
#endif
        return apply(apply(apply(apply(ba.map_(fa), bb), bc), bd), be);
    }

#if defined(SODIUM_NO_CXX11)
    namespace impl {
        template <class A, class P>
        struct split_post : i_lambda0<void> {
            split_post(const std::list<A>& la, const SODIUM_SHARED_PTR<impl::node>& target)
            : la(la), target(target) {}
            std::list<A> la;
            SODIUM_SHARED_PTR<impl::node> target;
            virtual void operator () () const {
                for (typename std::list<A>::iterator it = la.begin(); it != la.end(); ++it) {
                    transaction<P> trans;
                    send(target, trans.impl(), light_ptr::create<A>(*it));
                }
            }
        };
        template <class A, class P>
        struct split_handler : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&> {
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) const {
                const std::list<A>& la = *ptr.cast_ptr<std::list<A> >(NULL);
                trans->part->post(new split_post<A,P>(la, target));
            }
        };
    }
#endif

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
        SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
        transaction<P> trans;
#if defined(SODIUM_NO_CXX11)
        lambda0<void>* kill = e.listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
            new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&>(
                new impl::split_handler<A, P>
            )
#else
        auto kill = e.listen_raw(trans.impl(), std::get<1>(p),
            new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                [] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                    const std::list<A>& la = *ptr.cast_ptr<std::list<A>>(NULL);
                    trans->part->post([la, target] () {
                        for (auto it = la.begin(); it != la.end(); ++it) {
                            transaction<P> trans;
                            send(target, trans.impl(), light_ptr::create<A>(*it));
                        }
                    });
                })
#endif
            , false);
        return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill);
    }
}  // end namespace sodium
#endif

