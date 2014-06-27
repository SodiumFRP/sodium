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
#include <vector>

#define SODIUM_CONSTANT_OPTIMIZATION

namespace sodium {
    template <class A, class P> class event;
    template <class A, class P> class behavior;
    template <class A, class P> class behavior_sink;
    template <class A, class P> class behavior_loop;
    template <class A, class P> class event_loop;
    template <class A, class B, class P EQ_DEF_PART>
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
        friend behavior<B, P> sodium::apply(const behavior<lambda1<B, const A&>, P>& bf, const behavior<A, P>& ba);
#else
        friend behavior<B, P> sodium::apply(const behavior<std::function<B(const A&)>, P>& bf, const behavior<A, P>& ba);
#endif
        template <class A, class P>
        friend event<A, P> sodium::filter_optional(const event<boost::optional<A>, P>& input);
        friend behavior_ apply(transaction_impl* trans0, const behavior_& bf, const behavior_& ba);
#if defined(NO_CXX11)
        friend event_ map_(transaction_impl* trans, const lambda1<light_ptr, const light_ptr&>& f, const event_& ev);
#else
        friend event_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f, const event_& ev);
#endif
        friend event_ switch_e(transaction_impl* trans, const behavior_& bea);
        template <class A, class P>
        friend event<A, P> sodium::split(const event<std::list<A>, P>& e);

        public:
#if defined(NO_CXX11)
            typedef lambda1<void, std::vector<light_ptr>&> sample_now_func;
#else
            typedef std::function<void(std::vector<light_ptr>&)> sample_now_func;
#endif

        protected:
            boost::intrusive_ptr<listen_impl_func<H_EVENT> > p_listen_impl;
            SODIUM_SHARED_PTR<sample_now_func> p_sample_now;

        public:
            event_();
            event_(const boost::intrusive_ptr<listen_impl_func<H_EVENT> >& p_listen_impl,
                   const SODIUM_SHARED_PTR<sample_now_func>& p_sample_now)
                : p_listen_impl(p_listen_impl), p_sample_now(p_sample_now) {}
            event_(const boost::intrusive_ptr<listen_impl_func<H_EVENT> >& p_listen_impl,
                   sample_now_func* p_sample_now)
                : p_listen_impl(p_listen_impl), p_sample_now(p_sample_now) {}

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            bool is_never() const { return !impl::alive(p_listen_impl); }
#endif

        protected:

            /*!
             * listen to events.
             */
#if defined(NO_CXX11)
            lambda0<void>* listen_raw(
#else
            std::function<void()>* listen_raw(
#endif
                        transaction_impl* trans0,
                        const SODIUM_SHARED_PTR<impl::node>& target,
#if defined(NO_CXX11)
                        lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>* handle,
#else
                        std::function<void(const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&)>* handle,
#endif
                        bool suppressEarlierFirings) const;

            /*!
             * This is far more efficient than add_cleanup because it modifies the event
             * in place.
             */
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
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
             * Create a new event that is like this event but has an extra cleanup.
             */
#if defined(NO_CXX11)
            event_ add_cleanup_(transaction_impl* trans, lambda0<void>* cleanup) const;
#else
            event_ add_cleanup_(transaction_impl* trans, std::function<void()>* cleanup) const;
#endif
            behavior_ hold_(transaction_impl* trans, const light_ptr& initA) const;
            event_ once_(transaction_impl* trans) const;
            event_ merge_(transaction_impl* trans, const event_& other) const;
#if defined(NO_CXX11)
            event_ coalesce_(transaction_impl* trans, const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine) const;
#else
            event_ coalesce_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const;
#endif
            event_ last_firing_only_(transaction_impl* trans) const;
#if defined(NO_CXX11)
            event_ snapshot_(transaction_impl* trans, const behavior_& beh, const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine) const;
            event_ filter_(transaction_impl* trans, const lambda1<bool, const light_ptr&>& pred) const;
#else
            event_ snapshot_(transaction_impl* trans, const behavior_& beh, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const;
            event_ filter_(transaction_impl* trans, const std::function<bool(const light_ptr&)>& pred) const;
#endif

            void sample_now(std::vector<light_ptr>& values) const {
                if (p_sample_now != NULL)
                    (*p_sample_now)(values);
            }

#if defined(NO_CXX11)
            lambda0<void>* listen_impl(
#else
            std::function<void()>* listen_impl(
#endif
                transaction_impl* trans,
                const SODIUM_SHARED_PTR<impl::node>& target,
#if defined(NO_CXX11)
                lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>* handler,
#else
                std::function<void(const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&)>* handler,
#endif
                bool suppressEarlierFirings) const
            {
                boost::intrusive_ptr<listen_impl_func<H_STRONG> > li(
                    reinterpret_cast<listen_impl_func<H_STRONG>*>(p_listen_impl.get()));
                if (alive(li))
                    return (*li->func)(trans, target, handler, suppressEarlierFirings);
                else {
                    delete handler;
                    return NULL;
                }
            }
        };
#if defined(NO_CXX11)
        template <class A, class B>
        class _de_type : public i_lambda1<light_ptr, const light_ptr&>
        {
        private:
            lambda1<B,A> f;
        public:
            _de_type(const lambda1<B,A>& f) : f(f) {}
            virtual light_ptr operator () (const light_ptr& a) const
            {
                return light_ptr::create<B>(f(*a.cast_ptr<A>(NULL)));
            }
        };
        #define SODIUM_DETYPE_FUNCTION1(A,B,f) sodium::impl::_de_type<A,B>(f)
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
            > unsafe_new_event(event_::sample_now_func* sample_now_func = NULL);

        struct behavior_impl {
            behavior_impl(const light_ptr& constant);
            behavior_impl(
                const event_& updates,
#if defined(NO_CXX11)
                const lambda0<light_ptr>& sample,
                lambda0<void>* kill,
#else
                const std::function<light_ptr()>& sample,
                std::function<void()>* kill,
#endif
                const SODIUM_SHARED_PTR<behavior_impl>& parent);
            ~behavior_impl();

            event_ updates;  // Having this here allows references to behavior to keep the
                             // underlying event's cleanups alive, and provides access to the
                             // underlying event, for certain primitives.

#if defined(NO_CXX11)
            lambda0<light_ptr> sample;
            lambda0<void>* kill;
#else
            std::function<light_ptr()> sample;
            std::function<void()>* kill;
#endif
            SODIUM_SHARED_PTR<behavior_impl> parent;

#if defined(NO_CXX11)
            lambda3<lambda0<void>, transaction_impl*, const SODIUM_SHARED_PTR<node>&,
                             const lambda2<void, transaction_impl*, const light_ptr&>&> listen_value_raw() const;
#else
            std::function<std::function<void()>(transaction_impl*, const SODIUM_SHARED_PTR<node>&,
                             const std::function<void(transaction_impl*, const light_ptr&)>&)> listen_value_raw() const;
#endif
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

#if defined(NO_CXX11)
        behavior_ map_(transaction_impl* trans, const lambda1<light_ptr, const light_ptr&>& f,
            const behavior_& beh);
#else
        behavior_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f,
            const behavior_& beh);
#endif

        template <class S>
        struct collect_state {
            collect_state(const S& s) : s(s) {}
            S s;
        };

#if defined(NO_CXX11)
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

#if defined(NO_CXX11)
    template <class A, class B>
    struct fst_arg : i_lambda2<A,A,B>  {
        virtual A operator () (const A& a, const B&) const { return a; }
    };
    template <class A, class B>
    struct snd_arg : i_lambda2<B,A,B> {
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
#if defined(NO_CXX11)
        friend behavior<BB, PP> apply(const behavior<lambda1<BB, const AA&>, PP>& bf, const behavior<AA, PP>& ba);
#else
        friend behavior<BB, PP> apply(const behavior<std::function<BB(const AA&)>, PP>& bf, const behavior<AA, PP>& ba);
#endif
        template <class AA, class PP>
        friend behavior<AA, PP> switch_b(const behavior<behavior<AA, PP>, PP>& bba);
        template <class AA, class PP>
        friend event<AA, PP> switch_e(const behavior<event<AA, PP>, PP>& bea);
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

            /*!
             * Sample the value of this behavior.
             */
            A sample() const {
                transaction<P> trans;
                return *impl->sample().template cast_ptr<A>(NULL);
            }

            /*!
             * Returns a new behavior with the specified cleanup added to it, such that
             * it will be executed when no copies of the new behavior are referenced.
             */
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
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
            behavior<B, P> collect(
                const S& initS,
#if defined(NO_CXX11)
                const lambda2<SODIUM_TUPLE<B, S>, const A&, const S&>& f
#else
                const std::function<std::tuple<B, S>(const A&, const S&)>& f
#endif
            ) const
            {
                transaction<P> trans;
#if defined(NO_CXX11)
                event<A, P> ea = updates().coalesce(lambda2<A,A,A>(new snd_arg<A,A>));
#else
                auto ea = updates().coalesce([] (const A&, const A& snd) -> A { return snd; });
#endif
                A za = sample();
                SODIUM_TUPLE<B,S> zbs = f(za, initS);
                SODIUM_SHARED_PTR<impl::collect_state<S> > pState(new impl::collect_state<S>(SODIUM_TUPLE_GET<1>(zbs)));
                SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
#if defined(NO_CXX11)
                lambda0<void>* kill = updates().listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
                    //new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&>(
                        new impl::collect_handler<A,S,B>(pState, f)
                    /*)*/, false);
#else
                auto kill = updates().listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
                    new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            SODIUM_TUPLE<B,S> outsSt = f(*ptr.cast_ptr<A>(NULL), pState->s);
                            pState->s = SODIUM_TUPLE_GET<1>(outsSt);
                            send(target, trans, light_ptr::create<B>(SODIUM_TUPLE_GET<0>(outsSt)));
                        }), false);
#endif
                return event<B, P>(SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill)).hold(SODIUM_TUPLE_GET<0>(zbs));
            }

    };  // end class behavior

#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
            lambda0<void> listen(const lambda1<void, const A&>& handle) const {
#else
            std::function<void()> listen(const std::function<void(const A&)>& handle) const {
#endif
                transaction<P> trans;
#if defined(NO_CXX11)
                lambda0<void>* pKill = listen_raw(trans.impl(),
#else
                std::function<void()>* pKill = listen_raw(trans.impl(),
#endif
                    SODIUM_SHARED_PTR<impl::node>(new impl::node(SODIUM_IMPL_RANK_T_MAX)),
#if defined(NO_CXX11)
                    //new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&>(
                        new impl::listen_wrap<A>(handle)
                    /*)*/, false);
#else
                    new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [handle] (const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl* trans, const light_ptr& ptr) {
                            handle(*ptr.cast_ptr<A>(NULL));
                        }), false);
#endif
                if (pKill != NULL) {
#if defined(NO_CXX11)
                    lambda0<void> kill(*pKill);
#else
                    std::function<void()> kill(*pKill);
#endif
                    delete pKill;
                    return kill;
                }
                else
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
            event<B, P> map_effectful(const lambda1<B, const A&>& f) const {
#else
            event<B, P> map_effectful(const std::function<B(const A&)>& f) const {
#endif
                return map_(f);  // Same as map() for now but this may change!
            }

            /*!
             * Map a function over this event to modify the output value.
             *
             * g++-4.7.2 has a bug where, under a 'using namespace std' it will interpret
             * b.template map<A>(f) as if it were std::map. If you get this problem, you can
             * work around it with map_.
             */
            template <class B>
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
            event<A, P> coalesce(const lambda2<A, const A&, const A&>& combine) const
#else
            event<A, P> coalesce(const std::function<A(const A&, const A&)>& combine) const
#endif
            {
                transaction<P> trans;
                return event<A, P>(coalesce_(trans.impl(),
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
            event<A, P> filter(const lambda1<bool, const A&>& pred) const
#else
            event<A, P> filter(const std::function<bool(const A&)>& pred) const
#endif
            {
                transaction<P> trans;
                return event<A, P>(filter_(trans.impl(),
#if defined(NO_CXX11)
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

            /*!
             * Sample the behavior's value as at the transaction before the
             * current one, i.e. no changes from the current transaction are
             * taken.
             */
            template <class B, class C>
#if defined(NO_CXX11)
            event<C, P> snapshot(const behavior<B, P>& beh, const lambda2<C, const A&, const B&>& combine) const
#else
            event<C, P> snapshot(const behavior<B, P>& beh, const std::function<C(const A&, const B&)>& combine) const
#endif
            {
                transaction<P> trans;
                return event<C, P>(snapshot_(trans.impl(), beh,
#if defined(NO_CXX11)
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
#if defined(NO_CXX11)
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
                return filter_optional<A>(snapshot<bool, boost::optional<A> >(
                    g,
#if defined(NO_CXX11)
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
            event<B, P> collect(
                const S& initS,
#if defined(NO_CXX11)
                const lambda2<SODIUM_TUPLE<B, S>, const A&, const S&>& f
#else
                const std::function<SODIUM_TUPLE<B, S>(const A&, const S&)>& f
#endif
            ) const
            {
                transaction<P> trans;
                SODIUM_SHARED_PTR<impl::collect_state<S> > pState(new impl::collect_state<S>(initS));
                SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
#if defined(NO_CXX11)
                lambda0<void>* kill = listen_raw(trans.impl(), SODIUM_TUPLE_GET<1>(p),
                    new impl::collect_handler<A,S,B>(pState, f), false);
#else
                auto kill = listen_raw(trans.impl(), std::get<1>(p),
                    new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            auto outsSt = f(*ptr.cast_ptr<A>(NULL), pState->s);
                            pState->s = std::get<1>(outsSt);
                            send(target, trans, light_ptr::create<B>(std::get<0>(outsSt)));
                        }), false);
#endif
                return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill);
            }

            template <class B>
            event<B, P> accum_e(
                const B& initB,
#if defined(NO_CXX11)
                const lambda2<B, const A&, const B&>& f
#else
                const std::function<B(const A&, const B&)>& f
#endif
            ) const
            {
                transaction<P> trans;
                SODIUM_SHARED_PTR<impl::collect_state<B> > pState(new impl::collect_state<B>(initB));
                auto p = impl::unsafe_new_event();
                auto kill = listen_raw(trans.impl(), std::get<1>(p),
                    new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                        [pState, f] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            pState->s = f(*ptr.cast_ptr<A>(NULL), pState->s);
                            send(target, trans, light_ptr::create<B>(pState->s));
                        }), false);
                return event<B, P>(std::get<0>(p).unsafe_add_cleanup(kill));
            }

            template <class B>
            behavior<B, P> accum(
                const B& initB,
#if defined(NO_CXX11)
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
             * Delays each event occurrence by putting it into a new transaction, using
             * the same method as split.
             */
            event<A, P> delay()
            {
                return split<A,P>(map_<std::list<A>>(
                        [] (const A& a) -> std::list<A> { return { a }; }
                    ));
            }

            /*!
             * Add a clean-up operation to be performed when this event is no longer
             * referenced.
             */
#if defined(NO_CXX11)
            event<A, P> add_cleanup(const lambda0<void>& cleanup) const
#else
            event<A, P> add_cleanup(const std::function<void()>& cleanup) const
#endif
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
        return cross<A, P, Q>(b.updates()).hold(b.sample());
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
            new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                [] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& poa) {
                    const boost::optional<A>& oa = *poa.cast_ptr<boost::optional<A>>(NULL);
                    if (oa) impl::send(target, trans, light_ptr::create<A>(oa.get()));
                }), false);
        return std::get<0>(p).unsafe_add_cleanup(kill);
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

            void send(const A& a) const
            {
                e.send(a);
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
    template <class A, class P EQ_DEF_PART>
    class event_loop : public event<A, P>
    {
        private:
            struct info {
                info(
                    const SODIUM_SHARED_PTR<impl::node>& target,
                    const SODIUM_SHARED_PTR<std::function<void()>*>& pKill
                )
                : target(target), pKill(pKill)
                {
                }
                SODIUM_SHARED_PTR<impl::node> target;
                SODIUM_SHARED_PTR<std::function<void()>*> pKill;
            };
            SODIUM_SHARED_PTR<info> i;

        private:
            event_loop(const impl::event_& ev, const SODIUM_SHARED_PTR<info>& i) : event<A, P>(ev), i(i) {}

        public:
            event_loop()
            {
                SODIUM_SHARED_PTR<std::function<void()>*> pKill(
                    new std::function<void()>*(new std::function<void()>(
                        [] () {
                            throw std::runtime_error("event_loop not looped back");
                        }
                    ))
                );
                auto p = impl::unsafe_new_event();
                *this = event_loop<A, P>(
                    std::get<0>(p).unsafe_add_cleanup(new std::function<void()>([pKill] () {
                        std::function<void()>* kill = *pKill;
                        if (kill)
                            (*kill)();
                        delete kill;
                    })),
                    SODIUM_SHARED_PTR<info>(new info(std::get<1>(p), pKill))
                );
            }

            void loop(const event<A, P>& e)
            {
                if (i) {
                    transaction<P> trans;
                    auto target(i->target);
                    *i->pKill = e.listen_raw(trans.impl(), target, NULL, false);
                    i = SODIUM_SHARED_PTR<info>();
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
    template <class A, class P EQ_DEF_PART>
    class behavior_loop : public behavior<A, P>
    {
        private:
            event_loop<A> elp;
            SODIUM_SHARED_PTR<std::function<light_ptr()>> pSample;

        public:
            behavior_loop()
                : behavior<A, P>(impl::behavior_()),
                  pSample(new std::function<light_ptr()>([] () -> light_ptr {
                      throw std::runtime_error("behavior_loop sampled before it was looped");
                  }))
            {
                auto pSample = this->pSample;
                this->impl = SODIUM_SHARED_PTR<impl::behavior_impl>(new impl::behavior_impl(
                    elp,
                    [pSample] () { return (*pSample)(); },
                    NULL,
                    SODIUM_SHARED_PTR<impl::behavior_impl>()));
            }

            void loop(const behavior<A, P>& b)
            {
                elp.loop(b.updates());
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

    /*!
     * Lift a binary function into behaviors.
     */
    template <class A, class B, class C, class P EQ_DEF_PART>
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
    template <class A, class B, class C, class D, class P EQ_DEF_PART>
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
    template <class A, class B, class C, class D, class E, class P EQ_DEF_PART>
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
            new std::function<void(const SODIUM_SHARED_PTR<impl::node>&, impl::transaction_impl*, const light_ptr&)>(
                [] (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
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

