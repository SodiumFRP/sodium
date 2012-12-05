#ifndef _SODIUM_SODIUM_H_
#define _SODIUM_SODIUM_H_

#include <functional>
#include <boost/optional.hpp>
#include <memory>

#define SODIUM_CONSTANT_OPTIMIZATION

namespace sodium {

    namespace impl {
        template <class A>
        struct ordered_value {
            ordered_value() : tid(-1) {}
            long long tid;
            boost::optional<A> oa;
        };

        struct ID {
            ID() : id(0) {}
            ID(unsigned id) : id(id) {}
            unsigned id;
            ID succ() const { return ID(id+1); }
            inline bool operator < (const ID& other) const { return id < other.id; }
        };

        class node
        {
            public:
                struct target {
                    target(
                        void* handler,
                        const std::shared_ptr<node>& target
                    ) : handler(handler),
                        target(target) {}
                    void* handler;
                    std::shared_ptr<node> target;
                };

            public:
                node() : rank(0) {}

                unsigned long long rank;
                std::list<node::target> targets;
                std::list<light_ptr> firings;

                void link(void* handler, const std::shared_ptr<node>& target);
                bool unlink(void* handler);

            private:
                void ensure_bigger_than(std::set<node*>& visited, unsigned long long limit);
        };

        unsigned long long rankOf(const std::shared_ptr<node>& target);

        struct untyped {
        };
    
        class event_ {
        public:
            typedef std::function<std::function<void()>(
                const transaction<untyped>&,
                const std::shared_ptr<impl::node>&,
                const std::function<void(const transaction<untyped>&, const light_ptr&)>&,
                const std::::shared_ptr<cleaner_upper>&)> listen;
    
        public:
            listen listen_impl_;
    
        private:
            std::::shared_ptr<cleaner_upper> cleanerUpper;
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            bool is_never_;
#endif
    
        public:
            event_();
            event_(const listen& listen_impl_);
            event_(const listen& listen_impl_, const std::::shared_ptr<cleaner_upper>& cleanerUpper
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
                        const transaction<untyped>& trans0,
                        const std::shared_ptr<impl::node>& target,
                        const std::function<void(const transaction<untyped>&, const light_ptr&)>& handle) const;
    
            /*!
             * The specified cleanup is performed whenever nobody is referencing this event
             * any more.
             */
            event_ add_cleanup(const std::function<void()>& newCleanup) const;
    
            const std::::shared_ptr<cleaner_upper>& get_cleaner_upper() const {
                return cleanerUpper;
            }
        };
    };  // end namespace impl

    template <class P, class A>
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
                    const transaction<P>& trans0,
                    const std::shared_ptr<impl::node>& target,
                    const std::function<void(const transaction<impl::untyped>&, const light_ptr&)>& handle) const
        {
            return listen_raw_(trans0.cast__((impl::untyped*)NULL), target, handle);
        }

        /*!
         * High-level interface to obtain an event's value.
         */
        std::function<void()> listen(const transaction<P>& trans0, std::function<void(const A&)> handle) const {
            return listen_raw(trans0, std::shared_ptr<impl::node>(), [handle] (const transaction<impl::untyped>& trans, const light_ptr& ptr) {
                handle(*ptr.castPtr<A>(NULL));
            });
        };

        /*!
         * High-level interface to obtain an event's value, variant that gives you the transaction.
         */
        std::function<void()> listen_trans(const transaction<P>& trans0, std::function<void(const transaction<P>&, const A&)> handle) const {
            return listen_raw(trans0, std::shared_ptr<impl::node>(), [handle] (const transaction<impl::untyped>& trans, const light_ptr& ptr) {
                handle(trans.cast__((P*)NULL), *ptr.castPtr<A>(NULL));
            });
        };
    };

    namespace impl {
        /*!
         * Creates an event, and a function to push a value into it.
         * Unsafe variant: Assumes 'push' is called on the partition's sequence.
         */
        std::tuple<
                event_,
                std::function<void(const transaction<untyped>&, const light_ptr&)>,
                std::shared_ptr<node>
            > unsafe_new_event();

        struct coalesce_state {
            boost::optional<light_ptr> oValue;
        };

        std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                            const std::function<void(const transaction<untyped>&, const light_ptr&)>&,
                                            const std::::shared_ptr<cleaner_upper>&)>
            coalesce_with_cu_impl(
                const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine,
                const std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                const std::function<void(const transaction<untyped>&, const light_ptr&)>&,
                                const std::::shared_ptr<cleaner_upper>&)>& listen_raw
            );

        /* Clean up the listener so if there are multiple firings per transaction, they're
           combined into one. */
        template <class A>
        inline std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                            const std::function<void(const transaction<untyped>&, const light_ptr&)>&,
                                            const std::::shared_ptr<cleaner_upper>&)>
            coalesce_with_cu(
                const std::function<A(const A&, const A&)>& combine,
                const std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                const std::function<void(const transaction<untyped>&, const light_ptr&)>&,
                                const std::::shared_ptr<cleaner_upper>&)>& listen_raw
            )
        {
            return coalesce_with_cu_impl(
                [combine] (const light_ptr& a, const light_ptr& b) -> light_ptr {
                    return light_ptr::create<A>(combine(*a.castPtr<A>(NULL), *b.castPtr<A>(NULL)));
                },
                listen_raw);
        }

        std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                            const std::function<void(const transaction<untyped>&, const light_ptr&)>&)>
            coalesce_with_impl(
                const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine,
                const std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                const std::function<void(const transaction<untyped>&, const light_ptr&)>&)>& listen_raw
            );

        /* Clean up the listener so if there are multiple firings per transaction, they're
           combined into one. */
        template <class A>
        inline std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                            const std::function<void(const transaction<untyped>&, const light_ptr&)>&)>
            coalesce_with(
                const std::function<A(const A&, const A&)>& combine,
                const std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                const std::function<void(const transaction<untyped>&, const light_ptr&)>&)>& listen_raw
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
        std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                            const std::function<void(const transaction<untyped>&, const light_ptr&)>&)>
            coalesce(
                const std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                const std::function<void(const transaction<untyped>&, const light_ptr&)>&)>& listen_raw
            );

        /* Clean up the listener so it gives only one value per transaction, specifically
           the last one. This is cut-and-pasted instead of being written in terms of coalesce_with
           because it's so commonly used, it's worth doing that to produce less template bloat.
         */
        std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                            const std::function<void(const transaction<untyped>&, const light_ptr&)>&,
                                            const std::::shared_ptr<cleaner_upper>&)>
            coalesce_cu(
                const std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                                const std::function<void(const transaction<untyped>&, const light_ptr&)>&,
                                const std::::shared_ptr<cleaner_upper>&)>& listen_raw
            );
    };

    /*!
     * If an event has multiple firings in one transaction, throw all away except for
     * the last of them.
     */
    template <class P, class A>
    event<P, A> coalesce(const event<P, A>& ea)
    {
        return event<P, A>(impl::coalesce_cu([ea] (const transaction<impl::untyped>& trans, const std::shared_ptr<impl::node>& target,
                                    const std::function<void(const transaction<impl::untyped>&, const light_ptr&)>& handle,
                                    const std::::shared_ptr<cleaner_upper>&)
                                                              -> std::function<void()> {
            return ea.listen_raw_(trans, target, handle);
        }));
    }

    /*!
     * If an event has multiple firings in one transaction, combine them into one.
     */
    template <class P, class A>
    event<P, A> coalesce_with(const std::function<A(const A&, const A&)>& combine, const event<P, A>& ea)
    {
        return event<P, A>(impl::coalesce_with_cu<A>(combine, ea.listen_impl_));
    }

    /*!
     * Creates an event, and a function to push a value into it.
     */
    template <class P, class A>
    std::tuple<event<P,A>, std::function<void(const transaction<P>&, const A&)>> new_event()
    {
        auto p = impl::unsafe_new_event();
        auto evt = std::get<0>(p);
        auto push = std::get<1>(p);
        return std::tuple<event<P,A>, std::function<void(const transaction<P>&, const A&)>>(
            event<P,A>(evt),
            [push] (const transaction<P>& trans, const A& a) {
                light_ptr ptr = light_ptr::create<A>(a);
                const transaction<impl::untyped>& uTrans(trans.cast__((impl::untyped*)NULL));
                trans.when_all_previous_committed([push, uTrans, ptr] () {
                    push(uTrans, ptr);
                });
            }
        );
    }

    template <class P, class A>
    event<P,A> merge(const transaction<P>& trans, const event<P,A>& one, const event<P,A>& two) {
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

    /*!
     * Adapt an event to a new event statefully, with the ability to output any number
     * of outputs for a given input.
     */
    template <class P, class S, class A, class B>
    event<P,B> collect_n(
        const transaction<P>& trans0,
        const event<P,A>& input,
        const S& initS,
        const std::function<std::tuple<List<B>, S>(const A&, const S&)>& f
    )
    {
        std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                    [pState, f, push] (const transaction<impl::untyped>& trans, const light_ptr& ptr) {
            auto outsSt = f(*ptr.castPtr<A>(NULL), pState->s);
            pState->s = std::get<1>(outsSt);
            for (auto outputs = std::get<0>(outsSt); outputs; outputs = outputs.tail())
                push(trans, light_ptr::create<B>(outputs.head()));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * Adapt an event to a new event statefully, with the ability to output any number
     * of outputs for a given input.
     */
    template <class P, class S, class A, class B>
    event<P,B> collect_n_transaction(
        const transaction<P>& trans0,
        const event<P,A>& input,
        const S& initS,
        const std::function<std::tuple<List<B>, S>(const transaction<P>&, const A&, const S&)>& f
    )
    {
        std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                    [pState, push, f] (const transaction<impl::untyped>& trans, const light_ptr& ptr) {
            auto outsSt = f(trans.cast__((P*)NULL), *ptr.castPtr<A>(NULL), pState->s);
            pState->s = std::get<1>(outsSt);
            for (auto outputs = std::get<0>(outsSt); outputs; outputs = outputs.tail())
                push(trans, light_ptr::create<B>(outputs.head()));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * Adapt an event to a new event statefully.  Always outputs one output for each
     * input.
     */
    template <class P, class S, class A, class B>
    event<P,B> collect(
        const transaction<P>& trans0,
        const event<P,A>& input,
        const S& initS,
        const std::function<std::tuple<B, S>(const A&, const S&)>& f
    )
    {
        std::shared_ptr<impl::collect_state<S>> pState(new impl::collect_state<S>(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                 [pState, push, f] (const transaction<impl::untyped>& trans, const light_ptr& ptr) {
            auto outsSt = f(*ptr.castPtr<A>(NULL), pState->s);
            pState->s = std::get<1>(outsSt);
            push(trans, light_ptr::create<B>(std::get<0>(outsSt)));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * Filter an event of optionals, keeping only the defined values.
     */
    template <class P, class A>
    event<P,A> filter_optional(const transaction<P>& trans0, const event<P,boost::optional<A>>& input)
    {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                           [push] (const transaction<impl::untyped>& trans, const light_ptr& poa) {
            const boost::optional<A>& oa = *poa.castPtr<boost::optional<A>>(NULL);
            if (oa) push(trans, light_ptr::create<A>(oa.get()));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * Filter this event based on the specified predicate.
     */
    template <class P, class A>
    event<P,A> filter_e(const transaction<P>& trans0, const std::function<bool(const A&)>& pred, const event<P,A>& input)
    {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                [pred, push] (transaction<impl::untyped> trans, const light_ptr& ptr) {
            if (pred(*ptr.castPtr<A>(NULL))) push(trans, ptr);
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    namespace impl {

        struct behavior_impl {
            behavior_impl();
            behavior_impl(const light_ptr& constant);
            behavior_impl(
                sch::Sequence* seq,
                const event_& changes,
                const std::function<boost::optional<light_ptr>()>& sample);

            smart_ptr<sch::Sequence> seq;
            event_ changes;  // Having this here allows references to behavior to keep the
                             // underlying event's cleanups alive, and provides access to the
                             // underlying event, for certain primitives.

            std::function<boost::optional<light_ptr>()> sample;

            std::function<std::function<void()>(const transaction<untyped>&, const std::shared_ptr<node>&,
                             const std::function<void(transaction<untyped>, const light_ptr&)>&)> listen_value_raw() const;
        };

        behavior_impl* hold(const transaction<untyped>& trans0,
                            const boost::optional<light_ptr>& initValue,
                            const event_& input);

        struct behavior_state {
            behavior_state(const boost::optional<light_ptr>& initA);
            ~behavior_state();
            boost::optional<light_ptr> current;
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
                    sch::Sequence* seq,
                    const event_& changes,
                    const std::function<boost::optional<light_ptr>()>& sample
                );
                std::shared_ptr<impl::behavior_impl> impl;

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                /*!
                 * For optimization, if this behavior is a constant, then return its value.
                 */
                boost::optional<light_ptr> getConstantValue() const;
#endif

                const std::function<boost::optional<light_ptr>()>& getSample() const {
                    return impl->sample;
                }

                std::function<void()> listen_value_raw(const transaction<impl::untyped>& trans, const std::shared_ptr<impl::node>& target,
                                   const std::function<void(const transaction<impl::untyped>&, const light_ptr&)>& handle) const {
                    return impl->listen_value_raw()(trans, target, handle);
                };
        };
    };  // end namespace impl

    /*!
     * A like an event, but it tracks the input event's current value and causes it
     * always to be output once at the beginning for each listener.
     */
    template <class P, class A>
    class behavior : public impl::behavior_ {
        template <class PP, class AA>
        friend event<PP,AA> changes(const behavior<PP,AA>& beh);
        private:
            behavior(const std::shared_ptr<impl::behavior_impl>& impl)
                : impl::behavior_(impl)
            {
            }

        public:
            behavior(
                sch::Sequence* seq,
                const event<P,A>& changes,
                const std::function<boost::optional<light_ptr>()>& sample
            )
                : impl::behavior_(seq, changes, sample)
            {
            }

            behavior(
                sch::Sequence* seq,
                const impl::event_& changes,
                const std::function<boost::optional<light_ptr>()>& sample,
                const impl::untyped*
            )
                : impl::behavior_(seq, changes, sample)
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
            void sample_raw(const transaction<P>& trans, const std::shared_ptr<impl::node>& target,
                    const std::function<void(const frp::transaction<P>&, const boost::optional<light_ptr>&)>& handle) const {
                const std::shared_ptr<impl::behavior_impl>& impl(this->impl);
                sch::mkevent<void>(trans.partition().sequence(), [impl, trans, target, handle] () {
                    auto oValue = impl->sample();
                    if (oValue)
                        handle(trans, oValue);
                    else {
                        // There are two ways to create a behavior with an initial value.
                        // Way 1: Pass an explicit initial value to a 'hold' constructor.
                        // Way 2: Push a value out the event in the initial transaction.
                        // In the second case, we may get here. In order to make sure we
                        // always catch the event, we have to wait till the end of the
                        // transaction. So we do that with on_phase_1_cleanup.
                        trans.on_phase_1_cleanup(rankOf(target), [impl, handle] (const std::shared_ptr<frp::impl::transaction_impl>& transImpl) -> void {
                            handle(transaction<P>(transImpl), impl->sample());
                        });
                    }
                })->schedule();
            }

            /*!
             * Sample the value of this behavior.
             */
            void sample(const transaction<P>& trans,
                    const std::function<void(const frp::transaction<P>&, const boost::optional<A>&)>& handle) const {
                return sample_raw(trans, std::shared_ptr<impl::node>(), [handle] (const frp::transaction<P>& trans, const boost::optional<light_ptr>& oPtr) {
                    handle(trans, oPtr ? boost::optional<A>(*oPtr.get().castPtr<A>(NULL))
                                       : boost::optional<A>());
                });
            }

            std::function<void()> listen_value_linked_raw(const transaction<P>& trans, const std::shared_ptr<impl::node>& target,
                               const std::function<void(const transaction<impl::untyped>&, const light_ptr&)>& handle) const {
                return impl::coalesce(impl->listen_value_raw())(trans.cast__((impl::untyped*)NULL), target, handle);
            };

            std::function<void()> listen_value_linked(const transaction<P>& trans, const std::shared_ptr<impl::node>& target,
                               const std::function<void(const transaction<P>&, const A&)>& handle) const {
                return impl::coalesce(impl->listen_value_raw())(trans.cast__((impl::untyped*)NULL), target,
                               [handle] (const transaction<impl::untyped>& trans, const light_ptr& ptr) {
                    handle(trans.cast__((P*)NULL), *ptr.castPtr<A>(NULL));
                });
            };

            /*!
             * listen to the underlying event, i.e. to updates.
             */
            std::function<void()> listen_raw(const transaction<P>& trans, const std::shared_ptr<impl::node>& target,
                                const std::function<void(transaction<impl::untyped>, const light_ptr&)>& handle) const {
                return impl->changes.listen_raw_(trans.cast__((impl::untyped*)NULL), target, handle);
            }

            behavior<P,A> add_cleanup(const std::function<void()>& newCleanup) const {
                return behavior<P,A>(std::shared_ptr<impl::behavior_impl>(
                        new impl::behavior_impl(impl->seq.get(), impl->changes.add_cleanup(newCleanup), impl->sample)));
            }
    };

    template <class P, class A>
    behavior<P,A> hold(const frp::transaction<P>& trans, const A& initA, const frp::event<P,A>& ev)
    {
        return behavior<P,A>(trans, boost::optional<A>(initA), ev);
    }

    /*!
     * Helper for creating a new_event and holding it.
     */
    template <class P, class A>
    std::tuple<behavior<P,A>, std::function<void(const transaction<P>&, const A&)>> new_behavior(
            const frp::transaction<P>& trans, const A& initA)
    {
        auto p = new_event<P,A>();
        return std::make_tuple(hold(trans, initA, std::get<0>(p)), std::get<1>(p));
    }

    /*!
     * Returns an event describing the changes in a behavior.
     */
    template <class P, class A>
    event<P,A> changes(const behavior<P,A>& beh) {return event<P,A>(beh.impl->changes);}

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
    template <class P, class A>
    event<P,A> values(const transaction<P>& trans0, const behavior<P,A>& beh) {
        auto p = impl::unsafe_new_event();
        auto out = std::get<0>(p);
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = beh.listen_value_linked_raw(trans0, target, push);
        return out.add_cleanup(kill);
    }

    /*!
     * Adapt a behavior to a new behavior statefully, with the ability to output any number
     * of outputs for a given input.
     */
    template <class P, class S, class A, class B>
    behavior<P,B> collect_n(
        const transaction<P>& trans0,
        const behavior<P,A>& input,
        const S& initS,                        // Initial state
        const boost::optional<B>& initOutput,  // Initial output value
        const std::function<std::tuple<List<B>, S>(const A&, const S&)>& f
    )
    {
        std::shared_ptr<S> state(new S(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_value_linked(trans0, target, [state, f, push] (const transaction<P>& trans, const A& a) {
            auto outsSt = f(a, *state);
            *state = std::get<1>(outsSt);
            for (auto outputs = std::get<0>(outsSt); outputs; outputs = outputs.tail())
                push(trans, outputs.head());
        });
        return behavior<P,B>(trans0, initOutput, std::get<0>(p).add_cleanup(kill));
    }

    /*!
     * Adapt a behavior to a new behavior statefully.  Always outputs one output for each
     * input.
     */
    template <class P, class S, class A, class B>
    behavior<P,B> collect(
        const transaction<P>& trans0,
        const behavior<P,A>& input,
        const S& initS,                        // Initial state
        const std::function<std::tuple<B, S>(const A&, const S&)>& f
    )
    {
        std::shared_ptr<S> state(new S(initS));
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_value_linked(trans0, target, [state, f, push] (const transaction<P>& trans, const A& a) {
            auto outsSt = f(a, *state);
            *state = std::get<1>(outsSt);
            push(trans.cast__((impl::untyped*)NULL), light_ptr::create<B>(std::get<0>(outsSt)));
        });
        return behavior<P,B>(trans0, boost::optional<B>(), std::get<0>(p).add_cleanup(kill));
    }

    /*!
     * Same pure semantics as fmap but suitable for code with effects.
     * Also supplies the transaction to the function.
     */
    template <class P, class A, class B>
    event<P,B> effectfully(
        const transaction<P>& trans0,
        const std::function<B(const transaction<P>&, const A&)>& f,
        const event<P,A>& input
    )
    {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = input.listen_raw(trans0, target,
                [push, f] (const transaction<impl::untyped>& trans, const light_ptr& ptr) {
            push(trans, light_ptr::create<B>(f(trans.cast__((P*)NULL), *ptr.castPtr<A>(NULL))));
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * behavior variant of effectfully.
     */
    template <class P, class A, class B>
    behavior<P,B> effectfully(
        const transaction<P>& trans0,
        const std::function<B(const transaction<P>&, const A&)>& f,
        const behavior<P,A>& input
    )
    {
        return behavior<P,B>(trans0, boost::optional<B>(), effectfully(trans0, f, values<M,A>(trans0, input)));
    }

    namespace impl {
        event_ fmap(const std::function<light_ptr(const light_ptr&)>& f,
            const event_& ca);
        behavior_ fmap(const std::function<light_ptr(const light_ptr&)>& f,
            const behavior_& beh);
    };

    #define FRP_DETYPE_FUNCTION1(A,B,f) \
                [f] (const light_ptr& a) -> light_ptr { \
                    return light_ptr::create<B>(f(*a.castPtr<A>(NULL))); \
                }

    /*!
     * Map a function over this event to modify the output value.
     */
    template <class P, class A, class B>
    event<P,B> fmap(const std::function<B(const A&)>& f, const event<P,A>& e) {
        return event<P,B>(impl::fmap(FRP_DETYPE_FUNCTION1(A,B,f), e));
    }

    /*!
     * Map a function over this behavior to modify the output value.
     */
    template <class P, class A, class B>
    behavior<P,B> fmap(const std::function<B(const A&)>& f, const behavior<P,A>& beh) {
        return behavior<P,B>(impl::fmap(FRP_DETYPE_FUNCTION1(A,B,f), beh));
    }

    namespace impl {
        behavior_ apply(const transaction<untyped>& trans0, const behavior_& bf, const behavior_& ba);
    };

    template <class P, class A, class B>
    behavior<P,B> apply(const transaction<P>& trans0, const behavior<P,std::function<B(const A&)>>& bf, const behavior<P,A>& ba) {
        return behavior<P,B>(impl::apply(
            trans0.cast__((impl::untyped*)NULL),
            impl::fmap([] (const light_ptr& pf) {
                const std::function<B(const A&)>& f = *pf.castPtr<std::function<B(const A&)>>(NULL);
                return light_ptr::create<std::function<light_ptr(const light_ptr&)>>(
                        FRP_DETYPE_FUNCTION1(A, B, f)
                    );
            }, bf),
            ba
        ));
    }

    /*!
     * Sample the behavior's value as at the transaction before the
     * current one, i.e. no changes from the current transaction are
     * taken.
     */
    template <class P, class A, class B, class C>
    event<P,C> snapshotWith(const transaction<P>& trans0,
        const std::function<C(A,B)>& combine,
        const event<P,A>& ev, const behavior<P,B>& beh)
    {
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = ev.listen_raw(trans0, target,
                [beh, push, combine, target] (const transaction<impl::untyped>& trans, const light_ptr& ptr) {
            beh.sample_raw(trans.cast__((P*)NULL), target,
                     [push, combine, ptr] (const transaction<P>& trans, const boost::optional<light_ptr>& ob) {
                if (ob)
                    push(trans.cast__((impl::untyped*)NULL), light_ptr::create<C>(combine(*ptr.castPtr<A>(NULL), *ob.get().castPtr<B>(NULL))));
            });
        });
        return std::get<0>(p).add_cleanup(kill);
    }

    /*!
     * Sample the behavior's value as at the transaction before the
     * current one, i.e. no changes from the current transaction are
     * taken.
     */
    template <class P, class A, class B>
    event<P,B> tag(const transaction<P>& trans0,
        const event<P,A>& ev, const behavior<P,B>& beh)
    {
        return snapshotWith<P, A, B, B>(trans0, [] (const A&, const B& b) { return b; }, ev, beh);
    }

    /*!
     * Allow events through only when the behavior's value is true.
     */
    template <class P, class A>
    frp::event<P,A> gate(const transaction<P>& trans0,
                       const frp::event<P,A>& input, const frp::behavior<P,bool>& gate)
    {
        return filter_optional<P,A>(trans0, snapshotWith<P, A, bool, boost::optional<A>>(
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
    template <class P, class A>
    std::tuple<event<P, A>, std::function<void(const transaction<P>&, const event<P,A>&)>> event_loop()
    {
        auto p = impl::unsafe_new_event();
        auto in = std::get<0>(p);
        auto pushIn = std::get<1>(p);
        auto target = std::get<2>(p);
        std::shared_ptr<std::function<void()>> pKill(
            new std::function<void()>(
                [] () {
                    THROW(InternalError, "event_loop not looped back");
                }
            )
        );
        return std::make_tuple(
            in.add_cleanup([pKill] () {
                std::function<void()> kill = *pKill;
                kill();
            }),
            [pKill, pushIn, target] (const transaction<P>& trans, const event<P,A>& out) {
                *pKill = out.listen_raw(trans, target, pushIn);
            }
        );
    }

    template <class P, class A>
    std::tuple<behavior<P,A>, std::function<void(const transaction<P>, const behavior<P,A>&)>>
        behavior_loop(const partition<P>& part)
    {
        auto p = event_loop<P,A>();
        auto in = std::get<0>(p);
        auto feedBack = std::get<1>(p);
        MVar<std::function<boost::optional<light_ptr>()>> mvSample;
        return std::make_tuple(
            behavior<P,A>(
                part.sequence(),
                in,
                [mvSample] () -> boost::optional<light_ptr> {
                    auto sample = mvSample.read();
                    return sample();
                }
            ),
            [feedBack, mvSample] (const transaction<P>& trans, const behavior<P,A>& out) {
                feedBack(trans, changes<P,A>(out));
                mvSample.put(out.getSample());
            }
        );
    }

    struct switch_e_state {
        std::map<long long, std::function<void()>> cleanups;
    };

    /*!
     * Flatten a behavior that contains an event to give an event that reflects
     * the current state of the behavior. Note that when an event is updated,
     * due to behavior's delay semantics, event occurrences for the new
     * event won't come through until the following transaction.
     */
    template <class P, class A>
    event<P,A> switch_e(const transaction<P>& trans0, const behavior<P,event<P,A>>& bea)
    {
        // Number each incoming event.
        behavior<P,std::tuple<long long, event<P,A>>> beaId = collect<P, long long, event<P,A>, std::tuple<long long, event<P,A>>>(
            trans0, bea, 1,
            [] (const event<P,A>& ea, long long nextID) {
                return std::tuple<std::tuple<long long, event<P,A>>, long long>(
                    std::tuple<long long, event<P,A>>(nextID, ea),
                    nextID+1
                );
            });

        std::shared_ptr<switch_e_state> pState(new switch_e_state);
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = beaId.listen_value_linked(trans0, target, [pState, beaId, push, target] (const transaction<P>& trans1,
                                                  const std::tuple<long long, event<P,A>>& pea) {
            auto ix = std::get<0>(pea);
            auto ea = std::get<1>(pea);
            auto unlisten = snapshotWith<P, A, std::tuple<long long, event<P,A>>, boost::optional<A>>(
                trans1, [ix] (const A& a, const std::tuple<long long, event<P,A>>& active) -> boost::optional<A> {
                    if (std::get<0>(active) == ix)
                        return boost::optional<A>(a);
                    else
                        return boost::optional<A>();
                }, ea, beaId).listen_raw(trans1, target,
                        [pState, push, ix] (const transaction<impl::untyped>& trans2, const light_ptr& poa) {
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
        Mutex mutex;
        long long activeIx;
        std::map<long long, std::function<void()>> cleanups;
    };

    /*!
     * behavior variant of switch.
     */
    template <class P, class A>
    behavior<P,A> switch_b(const transaction<P>& trans0, const behavior<P,behavior<P,A>>& bba)
    {
        std::shared_ptr<switch_b_state> pState(new switch_b_state);
        auto p = impl::unsafe_new_event();
        auto push = std::get<1>(p);
        auto target = std::get<2>(p);
        auto kill = bba.listen_value_linked(trans0, target, [pState, push, target] (const transaction<P>& trans1, const behavior<P,A>& ba) {
            long long ix;
            {
                MutexLock ml(pState->mutex);
                ix = ++pState->activeIx;
            }
            auto unlisten = ba.listen_value_linked_raw(trans1, target,
                    [pState, push, ix, target] (const transaction<impl::untyped>& trans2, const light_ptr& pa) {
                long long activeIx;
                std::list<std::function<void()>> cleanups;
                {
                    MutexLock ml(pState->mutex);
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
                MutexLock ml(pState->mutex);
                pState->cleanups[ix] = unlisten;
            }
        });
        return behavior<P,A>(trans0, boost::optional<A>(), std::get<0>(p).add_cleanup(kill));
    }

    template <class P, class A, class B, class C>
    behavior<P,C> lift2(const transaction<P>& trans0,
            const std::function<C(const A&, const B&)>& f, const behavior<P,A>& ba, const behavior<P,B>& bb)
    {
        std::function<std::function<C(const B&)>(const A&)> fa(
            [f] (const A& a) -> std::function<C(const B&)> {
                return [f, a] (const B& b) -> C { return f(a, b); };
            }
        );
        return apply<P, B, C>(trans0, fmap(fa, ba), bb);
    }

    template <class P, class A, class B, class C, class D>
    behavior<P,D> lift3(const transaction<P>& trans0,
        const std::function<D(const A&, const B&, const C&)>& f,
        const behavior<P,A>& ba,
        const behavior<P,B>& bb,
        const behavior<P,C>& bc
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
        return apply(trans0, apply(trans0, fmap(fa, ba), bb), bc);
    }

    /*!
     * Only let the first occurrence of the event through.
     */
    template <class P, class A>
    event<P,A> once(const transaction<P>& trans0, const event<P,A>& input) {
        return collect_n<P,bool,A,A>(trans0, input, true, [] (A a, bool open) -> std::tuple<List<A>, bool> {
            if (open)
                return std::tuple<List<A>, bool>(List<A>(a, List<A>()), false);
            else
                return std::tuple<List<A>, bool>(List<A>(), false);
        });
    }

    /*!
     * Convert a list of behaviors of A into a behavior containing a list of A.
     */
    template <class P, class A>
    behavior<P,List<A>> append(
            const transaction<P>& trans0, const List<frp::behavior<P,A>>& behs) {
        return foldr<frp::behavior<P,List<A>>, frp::behavior<P,A>>(
            [trans0] (const frp::behavior<P,A>& bX, const frp::behavior<P,List<A>>& bXS) {
                return lift2<P, A, List<A>, List<A>>(trans0, [] (const A& x, const List<A>& xs) {
                    return x %= xs;
                }, bX, bXS);
            },
            frp::behavior<P,List<A>>(List<A>()),
            behs
        );
    }

    /*!
     * Split an input event into multiple events, each output event being in a new
     * transaction.
     */
    template <class P, class A, class B>
    event<P,B> bifurcate(
        const transaction<P>& trans0,
        const std::function<List<B>(const A& a)>& f,
        const frp::event<P,A>& e
    ) {
        auto p = new_event<P,B>();
        auto push = std::get<1>(p);
        auto kill = e.listen_trans(trans0, [push, f] (const transaction<P>& trans, const A& a) {
            for (auto bs = f(a); bs; bs = bs.tail())
                push(transaction<P>(trans.partition()), bs.head());
        });
        return std::get<0>(p).add_cleanup(kill);
    }

};
#endif

