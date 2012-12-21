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
    
        private:
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
        };
        #define FRP_DETYPE_FUNCTION1(A,B,f) \
                   [f] (const light_ptr& a) -> light_ptr { \
                        return light_ptr::create<B>(f(*a.castPtr<A>(NULL))); \
                   }

        event_ map_(const std::function<light_ptr(const light_ptr&)>& f,
            const event_& ca);
    };  // end namespace impl

    template <class A>
    class event : public impl::event_ {
        public:
            /*!
             * The 'never' event (that never fires).
             */
            event() {}
            event(const listen& listen)
                : impl::event_(listen) {}
    
            event(const impl::event_& ev) : impl::event_(ev) {}

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
    };

    namespace impl {
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
    };

    /*!
     * If an event has multiple firings in one transaction, throw all away except for
     * the last of them.
     */
    template <class A>
    event<A> coalesce(const event<A>& ea)
    {
        return event<A>(impl::coalesce_cu([ea] (impl::transaction_impl* trans, const std::shared_ptr<impl::node>& target,
                                    const std::function<void(impl::transaction_impl*, const light_ptr&)>& handle,
                                    const std::shared_ptr<cleaner_upper>&)
                                                              -> std::function<void()> {
            return ea.listen_raw_(trans, target, handle);
        }));
    }

    /*!
     * If an event has multiple firings in one transaction, combine them into one.
     */
    template <class A>
    event<A> coalesce_with(const std::function<A(const A&, const A&)>& combine, const event<A>& ea)
    {
        return event<A>(impl::coalesce_with_cu<A>(combine, ea.listen_impl_));
    }

    /*!
     * Creates an event, and a function to push a value into it.
     */
    template <class A>
    std::tuple<event<A>, std::function<void(const A&)>> new_event()
    {
        auto p = impl::unsafe_new_event();
        auto evt = std::get<0>(p);
        auto push = std::get<1>(p);
        return std::tuple<event<A>, std::function<void(const A&)>>(
            event<A>(evt),
            [push] (const A& a) {
                light_ptr ptr = light_ptr::create<A>(a);
                transaction trans;
                push(trans.impl(), ptr);
            }
        );
    }

    template <class A>
    event<A> merge(impl::transaction_impl* trans, const event<A>& one, const event<A>& two) {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill_one = one.listen_raw(trans, target, push);
        auto kill_two = two.listen_raw(trans, target, push);
        return std::get<0>(p).add_cleanup([kill_one, kill_two] () {
            kill_one();
            kill_two();
        });
    }

    namespace impl {
        template <class S>
        struct collect_state {
            collect_state(const S& s) : s(s) {}
            S s;
        };
    }

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
     * Adapt an event to a new event statefully.  Always outputs one output for each
     * input.
     */
    template <class S, class A, class B>
    event<B> collect(
        impl::transaction_impl* trans0,
        const event<A>& input,
        const S& initS,
        const std::function<std::tuple<B, S>(const A&, const S&)>& f
    )
    {
        std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                 [pState, push, f] (impl::transaction_impl* trans, const light_ptr& ptr) {
            auto outsSt = f(*ptr.castPtr<A>(NULL), pState->s);
            pState->s = std::get<1>(outsSt);
            push(trans, light_ptr::create<B>(std::get<0>(outsSt)));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * Filter an event of optionals, keeping only the defined values.
     */
    template <class A>
    event<A> filter_optional(impl::transaction_impl* trans0, const event<boost::optional<A>>& input)
    {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                           [push] (impl::transaction_impl* trans, const light_ptr& poa) {
            const boost::optional<A>& oa = *poa.castPtr<boost::optional<A>>(NULL);
            if (oa) push(trans, light_ptr::create<A>(oa.get()));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * Filter this event based on the specified predicate.
     */
    template <class A>
    event<A> filter_e(impl::transaction_impl* trans0, const std::function<bool(const A&)>& pred, const event<A>& input)
    {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                [pred, push] (impl::transaction_impl* trans, const light_ptr& ptr) {
            if (pred(*ptr.castPtr<A>(NULL))) push(trans, ptr);
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    namespace impl {

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
        };

        behavior_ map_(const std::function<light_ptr(const light_ptr&)>& f,
            const behavior_& beh);
    };  // end namespace impl

    /*!
     * A like an event, but it tracks the input event's current value and causes it
     * always to be output once at the beginning for each listener.
     */
    template <class A>
    class behavior : public impl::behavior_ {
        template <class AA>
        friend event<AA> changes(const behavior<AA>& beh);
        private:
            behavior(const std::shared_ptr<impl::behavior_impl>& impl)
                : impl::behavior_(impl)
            {
            }

        public:
            behavior(
                const event<A>& changes,
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
                return impl->sample().template castPtr<A>(NULL);
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
    };

    template <class A>
    behavior<A> hold(const A& initA, const event<A>& ev)
    {
        transaction trans;
        return behavior<A>(trans.impl(), boost::optional<A>(initA), ev);
    }

    /*!
     * Helper for creating a new_event and holding it.
     */
    template <class A>
    std::tuple<behavior<A>, std::function<void(impl::transaction_impl*, const A&)>> new_behavior(
            impl::transaction_impl* trans, const A& initA)
    {
        auto p = new_event<A>();
        return std::make_tuple(hold(trans, initA, std::get<0>(p)), std::get<1>(p));
    }

    /*!
     * Returns an event describing the changes in a behavior.
     */
    template <class A>
    event<A> changes(const behavior<A>& beh) {return event<A>(beh.impl->changes);}

    namespace impl {
        /*!
         * Returns an event describing the changes in a behavior.
         */
        inline impl::event_ underlyingevent_(const impl::behavior_& beh) {return beh.impl->changes;}
    };

    /*!
     * Returns an event describing the value of a behavior, where there's an initial event
     * giving the current value.
     */
    template <class A>
    event<A> values(impl::transaction_impl* trans0, const behavior<A>& beh) {
        auto p = impl::unsafe_new_event();
        auto out = std::get<0>(p);
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = beh.listen_value_linked_raw(trans0, target, push);
        return out.add_cleanup(kill);
    }

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
        return behavior<B>(trans0, boost::optional<B>(), effectfully(trans0, f, values<A>(trans0, input)));
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
     * Sample the behavior's value as at the transaction before the
     * current one, i.e. no changes from the current transaction are
     * taken.
     */
    template <class A, class B, class C>
    event<C> snapshotWith(impl::transaction_impl* trans0,
        const std::function<C(A,B)>& combine,
        const event<A>& ev, const behavior<B>& beh)
    {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = ev.listen_raw(trans0, target,
                [beh, push, combine, target] (impl::transaction_impl* trans, const light_ptr& ptr) {
            beh.sample_raw(trans, target,
                     [push, combine, ptr] (impl::transaction_impl* trans, const boost::optional<light_ptr>& ob) {
                if (ob)
                    push(trans, light_ptr::create<C>(combine(*ptr.castPtr<A>(NULL), *ob.get().castPtr<B>(NULL))));
            });
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * Sample the behavior's value as at the transaction before the
     * current one, i.e. no changes from the current transaction are
     * taken.
     */
    template <class A, class B>
    event<B> tag(impl::transaction_impl* trans0,
        const event<A>& ev, const behavior<B>& beh)
    {
        return snapshotWith<A, B, B>(trans0, [] (const A&, const B& b) { return b; }, ev, beh);
    }

    /*!
     * Allow events through only when the behavior's value is true.
     */
    template <class A>
    event<A> gate(impl::transaction_impl* trans0,
                  const event<A>& input, const behavior<bool>& gate)
    {
        return filter_optional<A>(trans0, snapshotWith<A, bool, boost::optional<A>>(
            trans0, [] (const A& a, const bool& gated) {
                return gated ? boost::optional<A>(a) : boost::optional<A>();
            },
            input, gate)
        );
    }

    /*!
     * Enable the construction of event loops, like this:
     *
     *   auto loop = event_loop<A>(seq);
     *   auto inEv = get<0>(loop);
     *   auto feedBack = get<1>(loop);
     *   auto outEv = doSomething(inEv);
     *   feedBack(outEv);  // Now doSomething's output event is fed back into its input
     *
     * To do: This won't get cleaned up properly, so please currently only use on
     *   things with application lifetime.
     */
    template <class A>
    std::tuple<event<A>, std::function<void(impl::transaction_impl*, const event<A>&)>> event_loop()
    {
        auto p = impl::unsafe_new_event();
        auto in = std::get<0>(p);
        auto pushIn = std::get<1>(p);
        auto target = std::get<2>(p);
        std::shared_ptr<std::function<void()>> pKill(
            new std::function<void()>(
                [] () {
                    throw std::exception("event_loop not looped back");
                }
            )
        );
        return std::make_tuple(
            in.add_cleanup([pKill] () {
                std::function<void()> kill = *pKill;
                kill();
            }),
            [pKill, pushIn, target] (impl::transaction_impl* trans, const event<A>& out) {
                *pKill = out.listen_raw(trans, target, pushIn);
            }
        );
    }

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
            auto unlisten = snapshotWith<A, std::tuple<long long, event<A>>, boost::optional<A>>(
                trans1, [ix] (const A& a, const std::tuple<long long, event<A>>& active) -> boost::optional<A> {
                    if (std::get<0>(active) == ix)
                        return boost::optional<A>(a);
                    else
                        return boost::optional<A>();
                }, ea, beaId).listen_raw(trans1, target,
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

#if 0
    /*!
     * Only let the first occurrence of the event through.
     */
    template <class A>
    event<A> once(impl::transaction_impl* trans0, const event<A>& input) {
        return collect_n<bool,A,A>(trans0, input, true, [] (A a, bool open) -> std::tuple<List<A>, bool> {
            if (open)
                return std::tuple<List<A>, bool>(List<A>(a, List<A>()), false);
            else
                return std::tuple<List<A>, bool>(List<A>(), false);
        });
    }
#endif

#if 0
    /*!
     * Convert a list of behaviors of A into a behavior containing a list of A.
     */
    template <class A>
    behavior<List<A>> append(
            impl::transaction_impl* trans0, const List<frp::behavior<A>>& behs) {
        return foldr<frp::behavior<List<A>>, frp::behavior<A>>(
            [trans0] (const frp::behavior<A>& bX, const frp::behavior<List<A>>& bXS) {
                return lift2<A, List<A>, List<A>>(trans0, [] (const A& x, const List<A>& xs) {
                    return x %= xs;
                }, bX, bXS);
            },
            frp::behavior<List<A>>(List<A>()),
            behs
        );
    }
#endif
};
#endif

