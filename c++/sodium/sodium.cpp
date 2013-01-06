/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * All rights reserved.
 *
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#include <sodium/sodium.h>

using namespace std;
using namespace boost;


namespace sodium {
#define GCC_VERSION (__GNUC__ * 10000 \
                   + __GNUC_MINOR__ * 100 \
                   + __GNUC_PATCHLEVEL__)

#if GCC_VERSION < 40700
#define WORKAROUND_GCC_46_BUG
#endif

    namespace impl {
#ifdef WORKAROUND_GCC_46_BUG
        struct Nulllistener {
            std::function<void()> operator () (transaction_impl*, const std::shared_ptr<impl::node>&,
                        const std::function<void(transaction_impl*, const light_ptr&)>&,
                        const std::shared_ptr<cleaner_upper>&) {
                return [] () {};
            }
        };
        
        // Save a bit of memory by having one global instance of the 'never' listener.
        static const Nulllistener& getNever()
        {
            static Nulllistener* l;
            if (l == NULL)
                l = new Nulllistener;
            return *l;
        }
    
        event_::event_()
            : listen_impl_(getNever())
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            , is_never_(true)
#endif
        {
        }
#else  // !WORKAROUND_GCC_46_BUG
        // Save a bit of memory by having one global instance of the 'never' listener.
        static const impl::event_::listen& getNever()
        {
            static impl::event_::listen* l;
            if (l == NULL)
                l = new impl::event_::listen(
                    [] (transaction_impl* trans,
                        const std::shared_ptr<impl::node>&,
                        const std::function<void(transaction_impl*, const light_ptr&)>&,
                        const std::shared_ptr<cleaner_upper>&)
                    {
                        return [] () {};
                    }
                );
            return *l;
        }

        event_::event_()
            : listen_impl_(getNever())
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            , is_never_(true)
#endif
        {
        }
#endif  // WORKAROUND_GCC_46_BUG
    
        event_::event_(const listen& listen_impl_)
            : listen_impl_(listen_impl_)
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            , is_never_(false)
#endif
        {
        }
    
        event_::event_(const listen& listen_impl_, const std::shared_ptr<cleaner_upper>& cleanerUpper
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , bool is_never_
#endif
            )
            : listen_impl_(listen_impl_), cleanerUpper(cleanerUpper)
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            , is_never_(is_never_)
#endif
        {
        }
    
        event_::event_(const listen& listen_impl_, const std::function<void()>& f
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , bool is_never_
#endif
            )
            : listen_impl_(listen_impl_), cleanerUpper(new cleaner_upper(f))
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            , is_never_(is_never_)
#endif
        {
        }
    
        /*!
         * listen to events.
         */
        std::function<void()> event_::listen_raw_(
                    transaction_impl* trans0,
                    const std::shared_ptr<impl::node>& target,
                    const std::function<void(transaction_impl*, const light_ptr&)>& handle) const
        {
            return listen_impl_(trans0, target, handle, cleanerUpper);
        }

        void touch(const cleaner_upper&)
        {
        }

        /*!
         * The specified cleanup is performed whenever nobody is referencing this event
         * any more.
         */
        event_ event_::add_cleanup(const std::function<void()>& newCleanup) const {
            const std::shared_ptr<cleaner_upper>& cleanerUpper = this->cleanerUpper;
            if (cleanerUpper)
                return event_(listen_impl_, [newCleanup, cleanerUpper] () {
                    newCleanup();
                    touch(*cleanerUpper);  // Keep the reference to the old clean-up
                }
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , is_never_
#endif
                );
            else
                return event_(listen_impl_, newCleanup
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , is_never_
#endif
                    );
        }

        event_ event_::once_() const
        {
            transaction trans0;
            auto p = impl::unsafe_new_event();
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
            std::shared_ptr<function<void()>*> ppKill(new function<void()>*(NULL));
            auto kill = listen_raw_(trans0.impl(), target,
                        [push, ppKill] (impl::transaction_impl* trans, const light_ptr& ptr) {
                push(trans, ptr);
                function<void()>*& pKill = *ppKill;
                if (pKill != NULL) {
                    (*pKill)();
                    delete pKill;
                    pKill = NULL;
                }
            });
            (*ppKill) = new std::function<void()>(kill);
            return std::get<0>(p).add_cleanup([ppKill] () {
                function<void()>*& pKill = *ppKill;
                if (pKill != NULL) {
                    (*pKill)();
                    delete pKill;
                    pKill = NULL;
                }
            });
        }

        event_ event_::merge_(const event_& other) const {
            transaction trans;
            auto p = impl::unsafe_new_event();
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
            auto kill_one = this->listen_raw_(trans.impl(), target, push);
            auto kill_two = other.listen_raw_(trans.impl(), target, push);
            return std::get<0>(p).add_cleanup([kill_one, kill_two] () {
                kill_one();
                kill_two();
            });
        }

        struct holder {
            holder(
                const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                const std::shared_ptr<cleaner_upper>& cleanerUpper
            ) : handle(handle), cleanerUpper(cleanerUpper) {}
            std::function<void(transaction_impl*, const light_ptr&)> handle;
            std::shared_ptr<cleaner_upper> cleanerUpper;
        };

        behavior_impl::behavior_impl(const light_ptr& constant)
            : changes(event_()),
              sample([constant] () { return constant; })
        {
        }

        behavior_impl::behavior_impl(
            const event_& changes,
            const std::function<light_ptr()>& sample)
        : changes(changes), sample(sample) {}

        /*!
         * Creates an event, and a function to push a value into it.
         * Unsafe variant: Assumes 'push' is called on the partition's sequence.
         */
        std::tuple<event_, std::function<void(transaction_impl*, const light_ptr&)>, std::shared_ptr<node>> unsafe_new_event()
        {
            std::shared_ptr<node> n(new node);
            return std::make_tuple(
                // The event
                event_(
                    [n] (transaction_impl* trans,
                            const std::shared_ptr<node>& target,
                            const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                            const std::shared_ptr<cleaner_upper>& cleanerUpper) {  // Register listener
                        std::list<light_ptr> firings;
                        void* h = new holder(handle, cleanerUpper);
                        {
                            auto part = partition_state::instance();
                            pthread_mutex_lock(&part->listeners_lock);
                            n->link(h, target);     
                            pthread_mutex_unlock(&part->listeners_lock);
                            firings = n->firings;
                        }
                        if (firings.begin() != firings.end())
                            for (auto it = firings.begin(); it != firings.end(); it++)
                                handle(trans, *it);
                        return [n, h] () {  // Unregister listener
                            auto part = partition_state::instance();
                            pthread_mutex_lock(&part->listeners_lock);
                            if (n->unlink(h)) {
                                pthread_mutex_unlock(&part->listeners_lock);
                                delete (holder*)h;
                            }
                            else
                                pthread_mutex_unlock(&part->listeners_lock);
                        };
                    }
                ),
                // Function to push a value into this event
                [n] (transaction_impl* trans, const light_ptr& ptr) {
                    int ifs = 0;
                    holder* fs[16];
                    std::list<holder*> fsOverflow;
                    {
                        if (n->firings.begin() == n->firings.end())
                            trans->last([n] () {
                                n->firings.clear();
                            });
                        n->firings.push_back(ptr);
                        auto it = n->targets.begin();
                        while (it != n->targets.end()) {
                            fs[ifs++] = (holder*)it->handler;
                            it++;
                            if (ifs == 16) {
                                while (it != n->targets.end()) {
                                    fsOverflow.push_back((holder*)it->handler);
                                    it++;
                                }
                                break;
                            }
                        }
                    }
                    for (int i = 0; i < ifs; i++)
                        fs[i]->handle(trans, ptr);
                    for (auto it = fsOverflow.begin(); it != fsOverflow.end(); ++it)
                        (*it)->handle(trans, ptr);
                },
                n
            );
        }

        behavior_state::behavior_state(const light_ptr& initA)
            : current(initA)
        {
        }
        
        behavior_state::~behavior_state()
        {
        }

        behavior_impl* hold(transaction_impl* trans0, const light_ptr& initValue, const event_& input)
        {
            auto p = unsafe_new_event();
            auto out = std::get<0>(p);
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            if (input.is_never())
                return new behavior_impl(input, [initValue] () { return initValue; });
            else {
#endif
                std::shared_ptr<behavior_state> state(new behavior_state(initValue));
                auto unlisten = input.listen_raw_(trans0, target,
                                                 [push, state] (transaction_impl* trans, const light_ptr& ptr) {
                    bool first = !state->update;
                    state->update = boost::optional<light_ptr>(ptr);
                    if (first)
                        trans->last([state] () {
                            state->current = state->update.get();
                            state->update = boost::optional<light_ptr>();
                        });
                    push(trans, ptr);
                });
                auto changes = out.add_cleanup(unlisten);
                auto sample = [state] () {
                    return state->current;
                };
                return new behavior_impl(changes, sample);
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            }
#endif
        }

        behavior_::behavior_()
        {
        }

        behavior_::behavior_(behavior_impl* impl)
            : impl(impl)
        {
        }

        behavior_::behavior_(const std::shared_ptr<behavior_impl>& impl)
            : impl(impl)
        {
        }

        behavior_::behavior_(const light_ptr& a)
            : impl(new behavior_impl(a))
        {
        }

        behavior_::behavior_(
            const event_& changes,
            const std::function<light_ptr()>& sample
        )
            : impl(new behavior_impl(changes, sample))
        {
        }

        event_ behavior_::values_() const
        {
            auto sample = impl->sample;
            auto listen = impl->changes.listen_impl_;
            return event_(
                [sample, listen] (
                    transaction_impl* trans,
                    const std::shared_ptr<impl::node>& target,
                    const std::function<void(transaction_impl*, const light_ptr&)>& handler,
                    const std::shared_ptr<cleaner_upper>& cleaner_upper
                ) {
                    light_ptr value = sample();
                    handler(trans, value);
                    return listen(trans, target, handler, cleaner_upper);
                },
                impl->changes.get_cleaner_upper()
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , impl->changes.is_never()
#endif
            );
        }

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
        /*!
         * For optimization, if this behavior is a constant, then return its value.
         */
        boost::optional<light_ptr> behavior_::getConstantValue() const
        {
            return impl->changes.is_never() ? boost::optional<light_ptr>(impl->sample())
                                            : boost::optional<light_ptr>();
        }
#endif

        /* Clean up the listener so if there are multiple firings per transaction, they're
           combined into one. */
        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&,
                                            const std::shared_ptr<cleaner_upper>&)>
            coalesce_with_cu_impl(
                const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine,
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                const std::function<void(transaction_impl*, const light_ptr&)>&,
                                const std::shared_ptr<cleaner_upper>&)>& listen_raw
            )
        {
            return [combine, listen_raw] (transaction_impl* trans, const std::shared_ptr<node>& target,
                                const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                                const std::shared_ptr<cleaner_upper>& cleanerUpper)
                                                                            -> std::function<void()> {
                std::shared_ptr<coalesce_state> pState(new coalesce_state);
                return listen_raw(trans, target, [handle, combine, pState, target] (transaction_impl* trans, const light_ptr& ptr) {
                    if (!pState->oValue) {
                        pState->oValue = boost::optional<light_ptr>(ptr);
                        trans->prioritized(target, [handle, pState] (transaction_impl* trans) {
                            if (pState->oValue) {
                                handle(trans, pState->oValue.get());
                                pState->oValue = boost::optional<light_ptr>();
                            }
                        });
                    }
                    else
                        pState->oValue = make_optional(combine(pState->oValue.get(), ptr));
                }, cleanerUpper);
            };
        }

        /* Clean up the listener so if there are multiple firings per transaction, they're
           combined into one. */
        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&)>
            coalesce_with_impl(
                const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine,
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                const std::function<void(transaction_impl*, const light_ptr&)>&)>& listen_raw
            )
        {
            return [combine, listen_raw] (transaction_impl* trans, const std::shared_ptr<node>& target,
                                const std::function<void(transaction_impl*, const light_ptr&)>& handle)
                                                                            -> std::function<void()> {
                std::shared_ptr<coalesce_state> pState(new coalesce_state);
                return listen_raw(trans, target, [handle, combine, pState, target] (transaction_impl* trans, const light_ptr& ptr) {
                    if (!pState->oValue) {
                        pState->oValue = boost::optional<light_ptr>(ptr);
                        trans->prioritized(target, [handle, pState] (transaction_impl* trans) {
                            if (pState->oValue) {
                                handle(trans, pState->oValue.get());
                                pState->oValue = boost::optional<light_ptr>();
                            }
                        });
                    }
                    else
                        pState->oValue = make_optional(combine(pState->oValue.get(), ptr));
                });
            };
        }

        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&)>
            coalesce(
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                                    const std::function<void(transaction_impl*, const light_ptr&)>&)>& listen_raw
            )
        {
            return [listen_raw] (transaction_impl* trans, const std::shared_ptr<node>& target,
                                const std::function<void(transaction_impl*, const light_ptr&)>& handle) -> std::function<void()> {
                std::shared_ptr<coalesce_state> pState(new coalesce_state);
                return listen_raw(trans, target, [handle, pState, target] (transaction_impl* trans, const light_ptr& ptr) {
                    bool first = !(bool)pState->oValue;
                    pState->oValue = boost::optional<light_ptr>(ptr);
                    if (first)
                        trans->prioritized(target, [handle, pState] (transaction_impl* trans) {
                            if (pState->oValue) {
                                handle(trans, pState->oValue.get());
                                pState->oValue = boost::optional<light_ptr>();
                            }
                        });
                });
            };
        }

        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                            const std::function<void(transaction_impl*, const light_ptr&)>&,
                                            const std::shared_ptr<cleaner_upper>&)>
            coalesce_cu(
                const std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                                                    const std::function<void(transaction_impl*, const light_ptr&)>&,
                                                    const std::shared_ptr<cleaner_upper>&)>& listen_raw
            )
        {
            return [listen_raw] (transaction_impl* trans, const std::shared_ptr<node>& target,
                                const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                                const std::shared_ptr<cleaner_upper>& cleanerUpper) -> std::function<void()> {
                std::shared_ptr<coalesce_state> pState(new coalesce_state);
                return listen_raw(trans, target, [handle, pState, target] (transaction_impl* trans, const light_ptr& ptr) {
                    bool first = !(bool)pState->oValue;
                    pState->oValue = boost::optional<light_ptr>(ptr);
                    if (first)
                        trans->prioritized(target, [handle, pState] (transaction_impl* trans) {
                            if (pState->oValue) {
                                handle(trans, pState->oValue.get());
                                pState->oValue = boost::optional<light_ptr>();
                            }
                        });
                }, cleanerUpper);
            };
        }

        std::function<std::function<void()>(transaction_impl*, const std::shared_ptr<node>&,
                         const std::function<void(transaction_impl*, const light_ptr&)>&)>
                                             behavior_impl::listen_value_raw() const
        {
            const event_& changes(this->changes);
            const std::function<boost::optional<light_ptr>()>& sample(this->sample);
            return [changes, sample] (transaction_impl* trans, const std::shared_ptr<node>& target,
                               const std::function<void(transaction_impl*, const light_ptr&)>& handle
                           ) -> std::function<void()> {
                auto oValue = sample();
                if (oValue)
                    handle(trans, oValue.get());
                return changes.listen_raw_(trans, target, handle);
            };
        }

        struct applicative_state {
            applicative_state() {}
            boost::optional<std::function<light_ptr(const light_ptr&)>> oF;
            boost::optional<light_ptr> oA;
            boost::optional<light_ptr> calcB() const {
                if (oF && oA)
                    return boost::optional<light_ptr>(oF.get()(oA.get()));
                else
                    return boost::optional<light_ptr>();
            }
        };

        behavior_ apply(transaction_impl* trans0, const behavior_& bf, const behavior_& ba)
        {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            boost::optional<light_ptr> ocf = bf.getConstantValue();
            if (ocf) { // function is constant
                auto f = *ocf.get().castPtr<std::function<light_ptr(const light_ptr&)>>(NULL);
                return impl::map_(f, ba);  // map optimizes to a constant where ba is constant
            }
            else {
                boost::optional<light_ptr> oca = ba.getConstantValue();
                if (oca) {  // 'a' value is constant but function is not
                    auto a = oca.get();
                    return impl::map_([a] (const light_ptr& pf) -> light_ptr {
                        const std::function<light_ptr(const light_ptr&)>& f =
                            *pf.castPtr<std::function<light_ptr(const light_ptr&)>>(NULL);
                        return f(a);
                    }, bf);
                }
                else {
#endif
                    // Non-constant case
                    std::shared_ptr<applicative_state> state(new applicative_state);

                    auto p = impl::unsafe_new_event();
                    auto push = std::get<1>(p);
                    auto target = std::get<2>(p);
                    auto update = [state, push] (transaction_impl* trans) {
                        auto oo = state->calcB();
                        if (oo)
                            push(trans, oo.get());
                    };
                    auto kill1 = bf.listen_value_raw(trans0, target,
                            [state, update] (transaction_impl* trans, const light_ptr& pf) {
                        state->oF = boost::make_optional(*pf.castPtr<std::function<light_ptr(const light_ptr&)>>(NULL));
                        update(trans);
                    });
                    auto kill2 = ba.listen_value_raw(trans0, target,
                            [state, update] (transaction_impl* trans, const light_ptr& pa) {
                        state->oA = boost::make_optional(pa);
                        update(trans);
                    });
                    auto f = *bf.impl->sample().castPtr<std::function<light_ptr(const light_ptr&)>>(NULL);
                    return behavior_(impl::hold(
                        trans0,
                        f(ba.impl->sample()),
                        std::get<0>(p).add_cleanup(kill1)
                                      .add_cleanup(kill2)
                    ));
    #if defined(SODIUM_CONSTANT_OPTIMIZATION)
                }
            }
    #endif
        }

    };  // end namespace impl

    namespace impl {

        /*!
         * Map a function over this event to modify the output value.
         */
        event_ map_(const std::function<light_ptr(const light_ptr&)>& f, const event_& ev)
        {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            if (ev.is_never())
                return event_();
            else
#endif
                return event_(
                    [f,ev] (transaction_impl* trans0,
                            std::shared_ptr<node> target,
                            const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                            const std::shared_ptr<cleaner_upper>& cu) {
                        return ev.listen_impl_(trans0, target, [handle, f] (transaction_impl* trans, const light_ptr& ptr) {
                            handle(trans, f(ptr));
                        }, cu);
                    },
                    ev.get_cleaner_upper()
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , false
#endif
                );
        }

        behavior_ map_(const std::function<light_ptr(const light_ptr&)>& f, const behavior_& beh) {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            auto ca = beh.getConstantValue();
            if (ca)
                return behavior_(f(ca.get()));
            else
#endif
                return behavior_(
                    map_(f, underlyingevent_(beh)),
                    [f, beh] () -> light_ptr {
                        return f(beh.impl->sample());
                    }
                );
        }
    };  // end namespace impl
};  // end namespace sodium
