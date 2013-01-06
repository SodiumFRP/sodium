/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * All rights reserved.
 *
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

    namespace impl {

        struct untyped {
        };
    
        class event_ {
        public:
            typedef std::function<std::function<void()>(
                transaction_impl* trans,
                const std::shared_ptr<impl::node>&,
                const std::function<void(transaction_impl*, const light_ptr&)>&,
                const std::shared_ptr<cleaner_upper>&)> listen;
    
        public:
            listen listen_impl_;
    
        protected:
            std::shared_ptr<cleaner_upper> cleanerUpper;
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            bool is_never_;
#endif
    
        public:
            event_();
            event_(const listen& listen_impl_);
            event_(const listen& listen_impl_, const std::shared_ptr<cleaner_upper>& cleanerUpper
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , bool is_never_
#endif
                );
            event_(const listen& listen_impl_, const std::function<void()>& f
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , bool is_never_
#endif
                );
    
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            bool is_never() const { return is_never_; }
#endif
    
            /*!
             * listen to events.
             */
            std::function<void()> listen_raw_(
                        transaction_impl* trans0,
                        const std::shared_ptr<impl::node>& target,
                        const std::function<void(transaction_impl*, const light_ptr&)>& handle) const;
    
            /*!
             * The specified cleanup is performed whenever nobody is referencing this event
             * any more.
             */
            event_ add_cleanup(const std::function<void()>& newCleanup) const;
    
            const std::shared_ptr<cleaner_upper>& get_cleaner_upper() const {
                return cleanerUpper;
            }
            
        protected:
            event_ once_() const;
            event_ merge_(const event_& other) const;
        };
        #define FRP_DETYPE_FUNCTION1(A,B,f) \
                   [f] (const light_ptr& a) -> light_ptr { \
                        return light_ptr::create<B>(f(*a.castPtr<A>(NULL))); \
                   }

        event_ map_(const std::function<light_ptr(const light_ptr&)>& f,
            const event_& ca);

        /*!
         * Creates an event, and a function to push a value into it.
         * Unsafe variant: Assumes 'push' is called on the partition's sequence.
         */
        std::tuple<
                event_,
                std::function<void(transaction_impl*, const light_ptr&)>,
                std::shared_ptr<node>
            > unsafe_new_event();

        struct coalesce_state {
            boost::optional<light_ptr> oValue;
        };

        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&,
                                            const std::shared_ptr<cleaner_upper>&)>
            coalesce_with_cu_impl(
                const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine,
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                const std::function<void(transaction_impl*, const light_ptr&)>&,
                                const std::shared_ptr<cleaner_upper>&)>& listen_raw
            );

        /* Clean up the listener so if there are multiple firings per transaction, they're
           combined into one. */
        template <class A>
        inline std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&,
                                            const std::shared_ptr<cleaner_upper>&)>
            coalesce_with_cu(
                const std::function<A(const A&, const A&)>& combine,
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                const std::function<void(transaction_impl*, const light_ptr&)>&,
                                const std::shared_ptr<cleaner_upper>&)>& listen_raw
            )
        {
            return coalesce_with_cu_impl(
                [combine] (const light_ptr& a, const light_ptr& b) -> light_ptr {
                    return light_ptr::create<A>(combine(*a.castPtr<A>(NULL), *b.castPtr<A>(NULL)));
                },
                listen_raw);
        }

        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&)>
            coalesce_with_impl(
                const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine,
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                const std::function<void(transaction_impl*, const light_ptr&)>&)>& listen_raw
            );

        /* Clean up the listener so if there are multiple firings per transaction, they're
           combined into one. */
        template <class A>
        inline std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&)>
            coalesce_with(
                const std::function<A(const A&, const A&)>& combine,
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                const std::function<void(transaction_impl*, const light_ptr&)>&)>& listen_raw
            )
        {
            return coalesce_with_cu_impl([combine] (
                const light_ptr& a, const light_ptr& b) -> light_ptr {
                    return light_ptr::create<A>(combine(*a.castPtr<A>(NULL), *b.castPtr<A>(NULL)));
                },
                listen_raw);
        }

        /* Clean up the listener so it gives only one value per transaction, specifically
           the last one. This is cut-and-pasted instead of being written in terms of coalesce_with
           because it's so commonly used, it's worth doing that to produce less template bloat.
         */
        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&)>
            coalesce(
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                const std::function<void(transaction_impl*, const light_ptr&)>&)>& listen_raw
            );

        /* Clean up the listener so it gives only one value per transaction, specifically
           the last one. This is cut-and-pasted instead of being written in terms of coalesce_with
           because it's so commonly used, it's worth doing that to produce less template bloat.
         */
        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&,
                                            const std::shared_ptr<cleaner_upper>&)>
            coalesce_cu(
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                const std::function<void(transaction_impl*, const light_ptr&)>&,
                                const std::shared_ptr<cleaner_upper>&)>& listen_raw
            );

        struct behavior_impl {
            behavior_impl(const light_ptr& constant);
            behavior_impl(
                const event_& changes,
                const std::function<light_ptr()>& sample);

            event_ changes;  // Having this here allows references to behavior to keep the
                             // underlying event's cleanups alive, and provides access to the
                             // underlying event, for certain primitives.

            std::function<light_ptr()> sample;

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
            friend impl::event_ underlyingevent_(const behavior_& beh);
            public:
                behavior_();
                behavior_(behavior_impl* impl);
                behavior_(const std::shared_ptr<behavior_impl>& impl);
                behavior_(const light_ptr& a);
                behavior_(
                    const event_& changes,
                    const std::function<light_ptr()>& sample
                );
                std::shared_ptr<impl::behavior_impl> impl;

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                /*!
                 * For optimization, if this behavior is a constant, then return its value.
                 */
                boost::optional<light_ptr> getConstantValue() const;
#endif

                std::function<void()> listen_value_raw(impl::transaction_impl* trans, const std::shared_ptr<impl::node>& target,
                                   const std::function<void(impl::transaction_impl*, const light_ptr&)>& handle) const {
                    return impl->listen_value_raw()(trans, target, handle);
                };

                event_ values_() const;
        };

        behavior_ map_(const std::function<light_ptr(const light_ptr&)>& f,
            const behavior_& beh);
    };  // end namespace impl

    template <class A>
    class event;

    /*!
     * A like an event, but it tracks the input event's current value and causes it
     * always to be output once at the beginning for each listener.
     */
    template <class A>
    class behavior : public impl::behavior_ {
        template <class AA>
        friend class event;
        private:
            behavior(const std::shared_ptr<impl::behavior_impl>& impl)
                : impl::behavior_(impl)
            {
            }

        protected:
#if 0
            behavior(
                const event_& changes,
                const std::function<boost::optional<light_ptr>()>& sample
            )
                : impl::behavior_(changes, sample)
            {
            }

            behavior(
                const impl::event_& changes,
                const std::function<boost::optional<light_ptr>()>& sample,
                const impl::untyped*
            )
                : impl::behavior_(changes, sample)
            {
            }
#endif
            behavior() {}

        public:
            /*!
             * Constant value.
             */
            behavior(const A& a)
                : impl::behavior_(light_ptr::create<A>(a))
            {
            }

            behavior(const behavior_& beh) : behavior_(beh) {}

            /*!
             * Sample the value of this behavior.
             */
            A sample() const {
                return *impl->sample().template castPtr<A>(NULL);
            }

            std::function<void()> listen_value_linked_raw(impl::transaction_impl* trans, const std::shared_ptr<impl::node>& target,
                               const std::function<void(impl::transaction_impl*, const light_ptr&)>& handle) const {
                return impl::coalesce(impl->listen_value_raw())(trans, target, handle);
            };

            std::function<void()> listen_value_linked(impl::transaction_impl* trans, const std::shared_ptr<impl::node>& target,
                               const std::function<void(impl::transaction_impl*, const A&)>& handle) const {
                return impl::coalesce(impl->listen_value_raw())(trans, target,
                               [handle] (impl::transaction_impl* trans, const light_ptr& ptr) {
                    handle(trans, *ptr.castPtr<A>(NULL));
                });
            };

            /*!
             * listen to the underlying event, i.e. to updates.
             */
            std::function<void()> listen_raw(impl::transaction_impl* trans, const std::shared_ptr<impl::node>& target,
                                const std::function<void(impl::transaction_impl*, const light_ptr&)>& handle) const {
                return impl->changes.listen_raw_(trans, target, handle);
            }

            behavior<A> add_cleanup(const std::function<void()>& newCleanup) const {
                return behavior<A>(std::shared_ptr<impl::behavior_impl>(
                        new impl::behavior_impl(impl->changes.add_cleanup(newCleanup), impl->sample)));
            }

            /*!
             * Map a function over this behaviour to modify the output value.
             */
            template <class B>
            behavior<B> map(const std::function<B(const A&)>& f) const {
                return behavior<B>(impl::map_(FRP_DETYPE_FUNCTION1(A,B,f), *this));
            }

            /*!
             * Map a function over this behaviour to modify the output value.
             *
             * g++-4.7.2 has a bug where, under a 'using namespace std' it will interpret
             * b.template map<A>(f) as if it were std::map. If you get this problem, you can
             * work around it with map_.
             */
            template <class B>
            behavior<B> map_(const std::function<B(const A&)>& f) const {
                return behavior<B>(impl::map_(FRP_DETYPE_FUNCTION1(A,B,f), *this));
            }

            /*!
             * Returns an event describing the changes in a behavior.
             */
            event<A> changes() const {
                return event<A>(impl->changes);
            }

            /*!
             * Returns an event describing the value of a behavior, where there's an initial event
             * giving the current value.
             */
            event<A> values() {
                return event<A>(values_());
            }
    };  // end class behavior

    namespace impl {
        template <class S>
        struct collect_state {
            collect_state(const S& s) : s(s) {}
            S s;
        };
    }

    template <class A>
    event<A> filter_optional(const event<boost::optional<A>>& input);

    template <class A>
    class event : public impl::event_ {
        public:
            /*!
             * The 'never' event (that never fires).
             */
            event() {}
            event(const listen& listen) : impl::event_(listen) {}
            event(const impl::event_& ev) : impl::event_(ev) {}
        private:
            event(const listen& listen, const std::shared_ptr<cleaner_upper>& cleanerUpper
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , bool is_never_
#endif
                )
            : impl::event_(listen, cleanerUpper
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , is_never_
#endif
                ) {}
        public:

            std::function<void()> listen_raw(
                        impl::transaction_impl* trans0,
                        const std::shared_ptr<impl::node>& target,
                        const std::function<void(impl::transaction_impl*, const light_ptr&)>& handle) const
            {
                return listen_raw_(trans0, target, handle);
            }

            /*!
             * High-level interface to obtain an event's value.
             */
            std::function<void()> listen(std::function<void(const A&)> handle) const {
                transaction trans;
                return listen_raw(trans.impl(), std::shared_ptr<impl::node>(), [handle] (impl::transaction_impl* trans, const light_ptr& ptr) {
                    handle(*ptr.castPtr<A>(NULL));
                });
            };
    
            /*!
             * Map a function over this event to modify the output value.
             */
            template <class B>
            event<B> map(const std::function<B(const A&)>& f) const {
                return event<B>(impl::map_(FRP_DETYPE_FUNCTION1(A,B,f), *this));
            }
    
            /*!
             * Map a function over this event to modify the output value.
             *
             * g++-4.7.2 has a bug where, under a 'using namespace std' it will interpret
             * b.template map<A>(f) as if it were std::map. If you get this problem, you can
             * work around it with map_.
             */
            template <class B>
            event<B> map_(const std::function<B(const A&)>& f) const {
                return event<B>(impl::map_(FRP_DETYPE_FUNCTION1(A,B,f), *this));
            }

            event<A> merge(const event<A>& other) const {
                return event<A>(merge_(other));
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
            event<A> coalesce(const std::function<A(const A&, const A&)>& combine) const
            {
                return event<A>(impl::coalesce_with_cu<A>(combine, listen_impl_), cleanerUpper
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                        , is_never_
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
            event<A> merge(const event<A>& other, const std::function<A(const A&, const A&)>& combine) const
            {
                return merge(other).coalesce(combine);
            }

            /*!
             * Filter this event based on the specified predicate, passing through values
             * where the predicate returns true.
             */
            event<A> filter(const std::function<bool(const A&)>& pred) const
            {
                transaction trans;
                auto p = impl::unsafe_new_event();
                auto push = std::get<1>(p);
                auto target = std::get<2>(p);
                auto kill = listen_raw(trans.impl(), target,
                        [pred, push] (impl::transaction_impl* trans, const light_ptr& ptr) {
                    if (pred(*ptr.castPtr<A>(NULL))) push(trans, ptr);
                });
                return std::get<0>(p).add_cleanup(kill);
            }

            behavior<A> hold(const A& initA) const
            {
                transaction trans;
                return behavior<A>(
                    std::shared_ptr<impl::behavior_impl>(impl::hold(trans.impl(), light_ptr::create<A>(initA), *this))
                );
            }

            /*!
             * Sample the behavior's value as at the transaction before the
             * current one, i.e. no changes from the current transaction are
             * taken.
             */
            template <class B, class C>
            event<C> snapshot(const behavior<B>& beh, const std::function<C(const A&, const B&)>& combine) const
            {
                transaction trans;
                auto p = impl::unsafe_new_event();
                auto push = std::get<1>(p);
                auto target = std::get<2>(p);
                auto kill = listen_raw(trans.impl(), target,
                        [beh, push, combine] (impl::transaction_impl* trans, const light_ptr& ptr) {
                    push(trans, light_ptr::create<C>(combine(*ptr.castPtr<A>(NULL), beh.sample())));
                });
                return std::get<0>(p).add_cleanup(kill);
            }

            /*!
             * Sample the behavior's value as at the transaction before the
             * current one, i.e. no changes from the current transaction are
             * taken.
             */
            template <class B>
            event<B> snapshot(const behavior<B>& beh) const
            {
                return snapshot<B, B>(beh, [] (const A&, const B& b) { return b; });
            }

            /*!
             * Allow events through only when the behavior's value is true.
             */
            event<A> gate(const behavior<bool>& g) const
            {
                transaction trans;
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
            event<B> collect(
                const S& initS,
                const std::function<std::tuple<B, S>(const A&, const S&)>& f
            ) const
            {
                transaction trans;
                std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(initS));
                auto p = impl::unsafe_new_event();
                auto push = std::get<1>(p);
                auto target = std::get<2>(p);
                auto kill = listen_raw(trans.impl(), target,
                         [pState, push, f] (impl::transaction_impl* trans, const light_ptr& ptr) {
                    auto outsSt = f(*ptr.castPtr<A>(NULL), pState->s);
                    pState->s = std::get<1>(outsSt);
                    push(trans, light_ptr::create<B>(std::get<0>(outsSt)));
                });
                return std::get<0>(p).add_cleanup(kill);
            }
            
            template <class B>
            event<B> accum(
                const B& initB,
                const std::function<B(const A&, const B&)>& f
            ) const
            {
                transaction trans;
                std::shared_ptr<impl::collect_state<B>> pState(new impl::collect_state<B>(initB));
                auto p = impl::unsafe_new_event();
                auto push = std::get<1>(p);
                auto target = std::get<2>(p);
                auto kill = listen_raw(trans.impl(), target,
                         [pState, push, f] (impl::transaction_impl* trans, const light_ptr& ptr) {
                    pState->s = f(*ptr.castPtr<A>(NULL), pState->s);
                    push(trans, light_ptr::create<B>(pState->s));
                });
                return std::get<0>(p).add_cleanup(kill);
            }

            event<int> countE() const
            {
                return accum<int>(0, [] (const A&, const int& total) -> int {
                    return total+1;
                });
            }

            behavior<int> count() const
            {
                return countE().hold(0);
            }

            event<A> once() const
            {
                return event<A>(once_());
            }

    };  // end class event

    template <class A>
    class event_sink : public event<A>
    {
        private:
            std::function<void(impl::transaction_impl*, const light_ptr&)> push;

        public:
            event_sink<A>()
            {
                auto p = impl::unsafe_new_event();
                *this = event_sink<A>(std::get<0>(p));
                push = std::get<1>(p);
            }
            event_sink(const impl::event_& ev) : event<A>(ev) {}

            void send(const A& a) const {
                light_ptr ptr = light_ptr::create<A>(a);
                transaction trans;
                push(trans.impl(), ptr);
            }
    };

#if 0
    /*!
     * Adapt an event to a new event statefully, with the ability to output any number
     * of outputs for a given input.
     */
    template <class S, class A, class B>
    event<B> collect_n(
        impl::transaction_impl* trans0,
        const event<A>& input,
        const S& initS,
        const std::function<std::tuple<List<B>, S>(const A&, const S&)>& f
    )
    {
        std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                    [pState, f, push] (impl::transaction_impl* trans, const light_ptr& ptr) {
            auto outsSt = f(*ptr.castPtr<A>(NULL), pState->s);
            pState->s = std::get<1>(outsSt);
            for (auto outputs = std::get<0>(outsSt); outputs; outputs = outputs.tail())
                push(trans, light_ptr::create<B>(outputs.head()));
        });
        return std::get<0>(p).add_cleanup(kill);
    }
#endif

#if 0
    /*!
     * Adapt an event to a new event statefully, with the ability to output any number
     * of outputs for a given input.
     */
    template <class S, class A, class B>
    event<B> collect_n_transaction(
        impl::transaction_impl* trans0,
        const event<A>& input,
        const S& initS,
        const std::function<std::tuple<List<B>, S>(impl::transaction_impl*, const A&, const S&)>& f
    )
    {
        std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                    [pState, push, f] (impl::transaction_impl* trans, const light_ptr& ptr) {
            auto outsSt = f(trans.cast__((P*)NULL), *ptr.castPtr<A>(NULL), pState->s);
            pState->s = std::get<1>(outsSt);
            for (auto outputs = std::get<0>(outsSt); outputs; outputs = outputs.tail())
                push(trans, light_ptr::create<B>(outputs.head()));
        });
        return std::get<0>(p).add_cleanup(kill);
    }
#endif

    /*!
     * Filter an event of optionals, keeping only the defined values.
     */
    template <class A>
    event<A> filter_optional(const event<boost::optional<A>>& input)
    {
        transaction trans;
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans.impl(), target,
                           [push] (impl::transaction_impl* trans, const light_ptr& poa) {
            const boost::optional<A>& oa = *poa.castPtr<boost::optional<A>>(NULL);
            if (oa) push(trans, light_ptr::create<A>(oa.get()));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    template <class A>
    class behavior_sink : public behavior<A>
    {
        private:
            event_sink<A> e;

            behavior_sink(const behavior<A>& beh) : behavior<A>(beh) {}

        public:
            behavior_sink(const A& initA)
            {
                *dynamic_cast<behavior<A>*>(this) = e.hold(initA);
            }

            void send(const A& a)
            {
                e.send(a);
            }
    };

    namespace impl {
        /*!
         * Returns an event describing the changes in a behavior.
         */
        inline impl::event_ underlyingevent_(const impl::behavior_& beh) {return beh.impl->changes;}
    };

#if 0
    /*!
     * Adapt a behavior to a new behavior statefully, with the ability to output any number
     * of outputs for a given input.
     */
    template <class S, class A, class B>
    behavior<B> collect_n(
        impl::transaction_impl* trans0,
        const behavior<A>& input,
        const S& initS,                        // Initial state
        const boost::optional<B>& initOutput,  // Initial output value
        const std::function<std::tuple<List<B>, S>(const A&, const S&)>& f
    )
    {
        std::shared_ptr<S> state(new S(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_value_linked(trans0, target, [state, f, push] (impl::transaction_impl* trans, const A& a) {
            auto outsSt = f(a, *state);
            *state = std::get<1>(outsSt);
            for (auto outputs = std::get<0>(outsSt); outputs; outputs = outputs.tail())
                push(trans, outputs.head());
        });
        return behavior<B>(trans0, initOutput, std::get<0>(p).add_cleanup(kill));
    }
#endif

    /*!
     * Adapt a behavior to a new behavior statefully.  Always outputs one output for each
     * input.
     */
    template <class S, class A, class B>
    behavior<B> collect(
        impl::transaction_impl* trans0,
        const behavior<A>& input,
        const S& initS,                        // Initial state
        const std::function<std::tuple<B, S>(const A&, const S&)>& f
    )
    {
        std::shared_ptr<S> state(new S(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_value_linked(trans0, target, [state, f, push] (impl::transaction_impl* trans, const A& a) {
            auto outsSt = f(a, *state);
            *state = std::get<1>(outsSt);
            push(trans, light_ptr::create<B>(std::get<0>(outsSt)));
        });
        return behavior<B>(trans0, boost::optional<B>(), std::get<0>(p).add_cleanup(kill));
    }

    /*!
     * Same pure semantics as map but suitable for code with effects.
     * Also supplies the transaction to the function.
     */
    template <class A, class B>
    event<B> effectfully(
        impl::transaction_impl* trans0,
        const std::function<B(impl::transaction_impl*, const A&)>& f,
        const event<A>& input
    )
    {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                [push, f] (impl::transaction_impl* trans, const light_ptr& ptr) {
            push(trans, light_ptr::create<B>(f(trans, *ptr.castPtr<A>(NULL))));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * behavior variant of effectfully.
     */
    template <class A, class B>
    behavior<B> effectfully(
        impl::transaction_impl* trans0,
        const std::function<B(impl::transaction_impl*, const A&)>& f,
        const behavior<A>& input
    )
    {
        return behavior<B>(trans0, boost::optional<B>(), effectfully(trans0, f, input.values()));
    }

    namespace impl {
        behavior_ apply(transaction_impl* trans, const behavior_& bf, const behavior_& ba);
    };

    template <class A, class B>
    behavior<B> apply(impl::transaction_impl* trans0, const behavior<std::function<B(const A&)>>& bf, const behavior<A>& ba) {
        return behavior<B>(impl::apply(
            trans0,
            bf.map_([] (const light_ptr& pf) {
                const std::function<B(const A&)>& f = *pf.castPtr<std::function<B(const A&)>>(NULL);
                return light_ptr::create<std::function<light_ptr(const light_ptr&)>>(
                        FRP_DETYPE_FUNCTION1(A, B, f)
                    );
            }),
            ba
        ));
    }

    /*!
     * Enable the construction of event loops, like this. This gives the ability to
     * forward reference an event.
     *
     *   event_loop<A> ea;
     *   auto ea_out = doSomething(ea);
     *   ea.loop(ea_out);  // ea is now the same as ea_out
     */
    template <class A>
    class event_loop : public event<A>
    {
        private:
            struct info {
                info(
                    const std::function<void(impl::transaction_impl*, const light_ptr&)>& pushIn,
                    const std::shared_ptr<impl::node>& target,
                    const std::shared_ptr<std::function<void()>>& pKill
                )
                : pushIn(pushIn), target(target), pKill(pKill)
                {
                }
                std::function<void(impl::transaction_impl*, const light_ptr&)> pushIn;
                std::shared_ptr<impl::node> target;
                std::shared_ptr<std::function<void()>> pKill;
            };
            std::shared_ptr<info> i;

        private:
            event_loop(const impl::event_& ev, const std::shared_ptr<info>& i) : event<A>(ev), i(i) {}

        public:
            event_loop() : i(NULL)
            {
                std::shared_ptr<std::function<void()>> pKill(
                    std::shared_ptr<std::function<void()>>(
                        new std::function<void()>(
                            [] () {
                                throw std::runtime_error("event_loop not looped back");
                            }
                        )
                    )
                );
                auto p = impl::unsafe_new_event();
                *this = event_loop<A>(
                    std::get<0>(p).add_cleanup([pKill] () {
                        std::function<void()> kill = *pKill;
                        kill();
                    }),
                    std::shared_ptr<info>(new info(std::get<1>(p), std::get<2>(p), pKill))
                );
            }

            void loop(const event<A>& e)
            {
                if (i) {
                    transaction trans;
                    *i->pKill = e.listen_raw(trans.impl(), i->target, i->pushIn);
                    i = std::shared_ptr<info>();
                }
                else
                    throw std::runtime_error("event_loop looped back more than once");
            }
    };

#if 0
    /*!
     * Enable the construction of event loops, like this:
     *
     *   auto loop = event_loop<A>();
     *   auto inEv = get<0>(loop);
     *   auto feedBack = get<1>(loop);
     *   auto outEv = doSomething(inEv);
     *   feedBack(outEv);  // Now doSomething's output event is fed back into its input
     */
    template <class A>
    std::tuple<event<A>, std::function<void(const event<A>&)>> event_loop()
    {
        auto p = impl::unsafe_new_event();
        auto in = std::get<0>(p);
        auto pushIn = std::get<1>(p);
        auto target = std::get<2>(p);
        std::shared_ptr<std::function<void()>> pKill(
            new std::function<void()>(
                [] () {
                    throw std::runtime_error("event_loop not looped back");
                }
            )
        );
        return std::make_tuple(
            in.add_cleanup([pKill] () {
                std::function<void()> kill = *pKill;
                kill();
            }),
            [pKill, pushIn, target] (const event<A>& out) {
                transaction trans;
                *pKill = out.listen_raw(trans.impl(), target, pushIn);
            }
        );
    }
#endif

#if 0
    template <class A>
    std::tuple<behavior<A>, std::function<void(impl::transaction_impl*, const behavior<A>&)>>
        behavior_loop()
    {
        auto p = event_loop<A>();
        auto in = std::get<0>(p);
        auto feedBack = std::get<1>(p);
        MVar<std::function<boost::optional<light_ptr>()>> mvSample;
        return std::make_tuple(
            behavior<A>(
                part.sequence(),
                in,
                [mvSample] () -> boost::optional<light_ptr> {
                    auto sample = mvSample.read();
                    return sample();
                }
            ),
            [feedBack, mvSample] (impl::transaction_impl* trans, const behavior<A>& out) {
                feedBack(trans, changes<A>(out));
                mvSample.put(out.getSample());
            }
        );
    }
#endif

    struct switch_e_state {
        std::map<long long, std::function<void()>> cleanups;
    };

    /*!
     * Flatten a behavior that contains an event to give an event that reflects
     * the current state of the behavior. Note that when an event is updated,
     * due to behavior's delay semantics, event occurrences for the new
     * event won't come through until the following transaction.
     */
    template <class A>
    event<A> switch_e(impl::transaction_impl* trans0, const behavior<event<A>>& bea)
    {
        // Number each incoming event.
        behavior<std::tuple<long long, event<A>>> beaId = collect<long long, event<A>, std::tuple<long long, event<A>>>(
            trans0, bea, 1,
            [] (const event<A>& ea, long long nextID) {
                return std::tuple<std::tuple<long long, event<A>>, long long>(
                    std::tuple<long long, event<A>>(nextID, ea),
                    nextID+1
                );
            });

        std::shared_ptr<switch_e_state> pState(new switch_e_state);
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = beaId.listen_value_linked(trans0, target, [pState, beaId, push, target] (impl::transaction_impl* trans1,
                                                  const std::tuple<long long, event<A>>& pea) {
            auto ix = std::get<0>(pea);
            auto ea = std::get<1>(pea);
            auto unlisten = ea.template snapshot<std::tuple<long long, event<A>>, boost::optional<A>>(
                beaId, [ix] (const A& a, const std::tuple<long long, event<A>>& active) -> boost::optional<A> {
                    if (std::get<0>(active) == ix)
                        return boost::optional<A>(a);
                    else
                        return boost::optional<A>();
                }).listen_raw(trans1, target,
                        [pState, push, ix] (impl::transaction_impl* trans2, const light_ptr& poa) {
                    const boost::optional<A>& oa = *poa.castPtr<boost::optional<A>>(NULL);
                    if (oa) {
                        push(trans2, light_ptr::create<A>(oa.get()));
                        // If we get a valid value at this ix, clean up any listeners that are older.
                        // (events will come in order due to listenOrdered).
                        while (pState->cleanups.begin() != pState->cleanups.end()
                                          && pState->cleanups.begin()->first < ix) {
                            std::function<void()> unlisten = pState->cleanups.begin()->second; 
                            unlisten();
                            pState->cleanups.erase(pState->cleanups.begin());
                        }
                    }
                });
            pState->cleanups[ix] = unlisten;
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    struct switch_b_state {
        switch_b_state() : activeIx(0) {}
        long long activeIx;
        std::map<long long, std::function<void()>> cleanups;
    };

    /*!
     * behavior variant of switch.
     */
    template <class A>
    behavior<A> switch_b(impl::transaction_impl* trans0, const behavior<behavior<A>>& bba)
    {
        std::shared_ptr<switch_b_state> pState(new switch_b_state);
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = bba.listen_value_linked(trans0, target, [pState, push, target] (impl::transaction_impl* trans1, const behavior<A>& ba) {
            long long ix;
            {
                ix = ++pState->activeIx;
            }
            auto unlisten = ba.listen_value_linked_raw(trans1, target,
                    [pState, push, ix, target] (impl::transaction_impl* trans2, const light_ptr& pa) {
                long long activeIx;
                std::list<std::function<void()>> cleanups;
                {
                    activeIx = pState->activeIx;
                    while (pState->cleanups.begin() != pState->cleanups.end()
                                      && pState->cleanups.begin()->first < activeIx) {
                        std::function<void()> unlisten = pState->cleanups.begin()->second;
                        cleanups.push_back(unlisten);
                        pState->cleanups.erase(pState->cleanups.begin());
                    }
                }
                for (auto it = cleanups.begin(); it != cleanups.end(); ++it)
                    (*it)();
                if (ix == activeIx)
                    push(trans2, pa);
            });
            {
                pState->cleanups[ix] = unlisten;
            }
        });
        return behavior<A>(trans0, boost::optional<A>(), std::get<0>(p).add_cleanup(kill));
    }

    template <class A, class B, class C>
    behavior<C> lift2(impl::transaction_impl* trans0,
            const std::function<C(const A&, const B&)>& f, const behavior<A>& ba, const behavior<B>& bb)
    {
        std::function<std::function<C(const B&)>(const A&)> fa(
            [f] (const A& a) -> std::function<C(const B&)> {
                return [f, a] (const B& b) -> C { return f(a, b); };
            }
        );
        return apply<B, C>(trans0, ba.map_(fa), bb);
    }

    template <class A, class B, class C, class D>
    behavior<D> lift3(impl::transaction_impl* trans0,
        const std::function<D(const A&, const B&, const C&)>& f,
        const behavior<A>& ba,
        const behavior<B>& bb,
        const behavior<C>& bc
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
        return apply(trans0, apply(trans0, ba.map_(fa), bb), bc);
    }
}  // end namespace sodium
#endif

