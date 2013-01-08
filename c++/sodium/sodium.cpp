/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
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
            : listen_impl_(getNever()),
              sample_now_([] (std::vector<light_ptr>&) {})
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
                        bool suppressEarlierFirings,
                        const std::shared_ptr<cleaner_upper>&)
                    {
                        return [] () {};
                    }
                );
            return *l;
        }

        event_::event_()
            : listen_impl_(getNever()),
              sample_now_([] (std::vector<light_ptr>&) {})
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            , is_never_(true)
#endif
        {
        }
#endif  // WORKAROUND_GCC_46_BUG

        event_::event_(const listen& listen_impl_, const sample_now& sample_now_,
                       const std::shared_ptr<cleaner_upper>& cleanerUpper
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , bool is_never_
#endif
            )
            : listen_impl_(listen_impl_),
              sample_now_(sample_now_),
              cleanerUpper(cleanerUpper)
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            , is_never_(is_never_)
#endif
        {
        }

        event_::event_(const listen& listen_impl_, const sample_now& sample_now_,
                       const std::function<void()>& f
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , bool is_never_
#endif
            )
            : listen_impl_(listen_impl_),
              sample_now_(sample_now_),
              cleanerUpper(new cleaner_upper(f))
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
                    const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                    bool suppressEarlierFirings) const
        {
            {
                vector<light_ptr> items;
                sample_now_(items);
                for (auto it = items.begin(); it != items.end(); ++it)
                    handle(trans0, *it);
            }
            return listen_impl_(trans0, target, handle, suppressEarlierFirings, cleanerUpper);
        }

        void touch(const cleaner_upper&)
        {
        }

        /*!
         * The specified cleanup is performed whenever nobody is referencing this event
         * any more.
         */
        event_ event_::add_cleanup_(const std::function<void()>& newCleanup) const {
            const std::shared_ptr<cleaner_upper>& cleanerUpper = this->cleanerUpper;
            if (cleanerUpper)
                return event_(listen_impl_, sample_now_, [newCleanup, cleanerUpper] () {
                    newCleanup();
                    touch(*cleanerUpper);  // Keep the reference to the old clean-up
                }
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , is_never_
#endif
                );
            else
                return event_(listen_impl_, sample_now_, newCleanup
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , is_never_
#endif
                    );
        }

        behavior_ event_::hold_(const light_ptr& initA) const
        {
            transaction trans;
            return behavior_(
                std::shared_ptr<impl::behavior_impl>(impl::hold(trans.impl(), initA, *this))
            );
        }

        event_ event_::once_() const
        {
            transaction trans0;
            std::shared_ptr<function<void()>*> ppKill(new function<void()>*(NULL));
            auto killOnce = [ppKill] () {
                function<void()>*& pKill = *ppKill;
                if (pKill != NULL) {
                    (*pKill)();
                    delete pKill;
                    pKill = NULL;
                }
            };
            auto sample_now = this->sample_now_;
            auto p = impl::unsafe_new_event([killOnce, sample_now] (vector<light_ptr>& items) {
                size_t start = items.size();
                sample_now(items);
                if (items.begin() + start != items.end()) {
                    auto it = items.begin();
                    ++it;
                    items.erase(it, items.end());
                    killOnce();
                }
            });
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
            auto kill = listen_raw_(trans0.impl(), target,
                        [push, killOnce] (impl::transaction_impl* trans, const light_ptr& ptr) {
                push(trans, ptr);
                killOnce();
            }, false);
            (*ppKill) = new std::function<void()>(kill);
            return std::get<0>(p).add_cleanup_(killOnce);
        }

        event_ event_::merge_(const event_& other) const {
            transaction trans;
            auto sample_me = sample_now_;
            auto sample_other = other.sample_now_;
            auto p = impl::unsafe_new_event([sample_me, sample_other] (std::vector<light_ptr>& items) {
                sample_me(items);
                sample_other(items);
            });
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
            auto kill_one = this->listen_raw_(trans.impl(), target, push, false);
            auto kill_two = other.listen_raw_(trans.impl(), target, push, false);
            return std::get<0>(p).add_cleanup_([kill_one, kill_two] () {
                kill_one();
                kill_two();
            });
        }

        event_ event_::coalesce_(const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const
        {
            auto sample_now_   = this->sample_now_;
            auto listen_impl_  = this->listen_impl_;
            return event_(
                [combine, listen_impl_] (transaction_impl* trans, const std::shared_ptr<node>& target,
                                const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                                bool suppressEarlierFirings,
                                const std::shared_ptr<cleaner_upper>& cleanerUpper)
                                                                            -> std::function<void()> {
                    std::shared_ptr<coalesce_state> pState(new coalesce_state);
                    return listen_impl_(
                        trans, target,
                        [handle, combine, pState, target] (transaction_impl* trans, const light_ptr& ptr) {
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
                        },
                        suppressEarlierFirings,
                        cleanerUpper);
                },
                [sample_now_, combine] (vector<light_ptr>& items) {
                    size_t start = items.size();
                    sample_now_(items);
                    auto first = items.begin() + start;
                    if (first != items.end()) {
                        auto it = first + 1;
                        if (it != items.end()) {
                            light_ptr sum = *first;
                            while (it != items.end())
                                sum = combine(sum, *it++);
                            items.erase(first, items.end());
                            items.push_back(sum);
                        }
                    }
                },
                cleanerUpper
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , is_never_
#endif
            );
        }
        
        event_ event_::last_firing_only_() const
        {
            return coalesce_([] (const light_ptr& fst, const light_ptr& snd) {
                return snd;
            });
        }

        /*!
         * Sample the behavior's value as at the transaction before the
         * current one, i.e. no changes from the current transaction are
         * taken.
         */
        event_ event_::snapshot_(const behavior_& beh, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const
        {
            transaction trans;
            auto sample_now = this->sample_now_;
            auto p = impl::unsafe_new_event([sample_now, combine, beh] (vector<light_ptr>& items) {
                size_t start = items.size();
                sample_now(items);
                for (auto it = items.begin() + start; it != items.end(); ++it)
                    *it = combine(*it, beh.impl->sample());
            });
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
            auto kill = listen_raw_(trans.impl(), target,
                    [beh, push, combine] (impl::transaction_impl* trans, const light_ptr& a) {
                push(trans, combine(a, beh.impl->sample()));
            }, false);
            return std::get<0>(p).add_cleanup_(kill);
        }

        /*!
         * Filter this event based on the specified predicate, passing through values
         * where the predicate returns true.
         */
        event_ event_::filter_(const std::function<bool(const light_ptr&)>& pred) const
        {
            transaction trans;
            auto sample_now = this->sample_now_;
            auto p = impl::unsafe_new_event([sample_now, pred] (vector<light_ptr>& output) {
                vector<light_ptr> input;
                sample_now(input);
                for (auto it = input.begin(); it != input.end(); ++it)
                    if (pred(*it))
                        output.push_back(*it);
            });
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
            auto kill = listen_raw_(trans.impl(), target,
                    [pred, push] (impl::transaction_impl* trans, const light_ptr& ptr) {
                if (pred(ptr)) push(trans, ptr);
            }, false);
            return std::get<0>(p).add_cleanup_(kill);
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
        std::tuple<event_, std::function<void(transaction_impl*, const light_ptr&)>, std::shared_ptr<node>> unsafe_new_event(
            const event_::sample_now& sample_now)
        {
            std::shared_ptr<node> n(new node);
            return std::make_tuple(
                // The event
                event_(
                    [n] (transaction_impl* trans,
                            const std::shared_ptr<node>& target,
                            const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                            bool suppressEarlierFirings,
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
                        if (!suppressEarlierFirings && firings.begin() != firings.end())
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
                    },
                    sample_now,
                    std::shared_ptr<cleaner_upper>()
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , false
#endif
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

        event_sink_impl::event_sink_impl()
        {
        }

        event_ event_sink_impl::construct()
        {
            auto p = impl::unsafe_new_event();
            this->push = std::get<1>(p);
            this->target = std::get<2>(p);
            return std::get<0>(p);
        }

        void event_sink_impl::send(const light_ptr& ptr) const
        {
            transaction trans;
            auto push(this->push);
            trans.impl()->prioritized(target, [push, ptr] (transaction_impl* trans_impl) {
                push(trans_impl, ptr);
            });
        }

        /*!
         * Creates an event, and a function to push a value into it.
         * Unsafe variant: Assumes 'push' is called on the partition's sequence.
         */
        std::tuple<event_, std::function<void(transaction_impl*, const light_ptr&)>, std::shared_ptr<node>> unsafe_new_event()
        {
            return unsafe_new_event([] (vector<light_ptr>&) {});
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
                }, false);
                auto changes = out.add_cleanup_(unlisten);
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
            return event_(
                changes_().listen_impl_,
                [sample] (vector<light_ptr>& items) { items.push_back(sample()); },
                changes_().get_cleaner_upper()
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                , changes_().is_never()
#endif
            ).last_firing_only_();
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
                auto f = *ocf.get().cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
                return impl::map_(f, ba);  // map optimizes to a constant where ba is constant
            }
            else {
                boost::optional<light_ptr> oca = ba.getConstantValue();
                if (oca) {  // 'a' value is constant but function is not
                    auto a = oca.get();
                    return impl::map_([a] (const light_ptr& pf) -> light_ptr {
                        const std::function<light_ptr(const light_ptr&)>& f =
                            *pf.cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
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
                    auto kill1 = bf.values_().listen_raw_(trans0, target,
                            [state, update] (transaction_impl* trans, const light_ptr& pf) {
                        state->oF = boost::make_optional(*pf.cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL));
                        update(trans);
                    }, false);
                    auto kill2 = ba.values_().listen_raw_(trans0, target,
                            [state, update] (transaction_impl* trans, const light_ptr& pa) {
                        state->oA = boost::make_optional(pa);
                        update(trans);
                    }, false);
                    auto f = *bf.impl->sample().cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
                    return behavior_(impl::hold(
                        trans0,
                        f(ba.impl->sample()),
                        std::get<0>(p).add_cleanup_(kill1)
                                      .add_cleanup_(kill2)
                    ));
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                }
            }
#endif
        }

        /*!
         * Map a function over this event to modify the output value.
         */
        event_ map_(const std::function<light_ptr(const light_ptr&)>& f, const event_& ev)
        {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            if (ev.is_never())
                return event_();
            else {
#endif
                auto sample_now = ev.sample_now_;
                return event_(
                    [f,ev] (transaction_impl* trans0,
                            std::shared_ptr<node> target,
                            const std::function<void(transaction_impl*, const light_ptr&)>& handle,
                            bool suppressEarlierFirings,
                            const std::shared_ptr<cleaner_upper>& cu) {
                        return ev.listen_impl_(trans0, target, [handle, f] (transaction_impl* trans, const light_ptr& ptr) {
                            handle(trans, f(ptr));
                        }, suppressEarlierFirings, cu);
                    },
                    [sample_now, f] (vector<light_ptr>& items) {
                        size_t start = items.size();
                        sample_now(items);
                        for (auto it = items.begin() + start; it != items.end(); ++it)
                            *it = f(*it);
                    },
                    ev.get_cleaner_upper()
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                    , false
#endif
                );
            }
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

        event_ switch_e(const behavior_& bea)
        {
            transaction trans0;
            auto p = unsafe_new_event();
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
            std::shared_ptr<optional<function<void()>>> pKillInner(new optional<function<void()>>(
                bea.impl->sample().cast_ptr<event_>(NULL)->listen_raw_(trans0.impl(), target, push, false)
            ));
            auto killInner = [pKillInner] () {
                if (*pKillInner) {
                    pKillInner->get()();
                    *pKillInner = optional<function<void()>>();
                }
            };
            auto killOuter = bea.changes_().listen_raw_(trans0.impl(), target,
                [killInner, pKillInner, target, push] (impl::transaction_impl* trans1, const light_ptr& pea) {
                    const event_& ea = *pea.cast_ptr<event_>(NULL);
                    trans1->last([killInner, pKillInner, ea, trans1, target, push] () {
                        killInner();
                        *pKillInner = ea.listen_raw_(trans1, target, push, true);
                    });
                },
                false
            );
            return std::get<0>(p).add_cleanup_(killInner).add_cleanup_(killOuter);
        }

        behavior_ switch_b(const behavior_& bba)
        {
            transaction trans0;
            light_ptr za = bba.impl->sample().cast_ptr<behavior_>(NULL)->impl->sample();
            std::shared_ptr<optional<function<void()>>> pKillInner(new optional<function<void()>>);
            auto killInner = [pKillInner] () {
                if (*pKillInner) {
                    pKillInner->get()();
                    *pKillInner = optional<function<void()>>();
                }
            };
            auto p = unsafe_new_event();
            auto push = std::get<1>(p);
            auto target = std::get<2>(p);
            auto killOuter = bba.values_().listen_raw_(trans0.impl(), target,
                [killInner, pKillInner, target, push] (transaction_impl* trans, const light_ptr& pa) {
                // Note: If any switch takes place during a transaction, then the
                // values().listen will always cause a sample to be fetched from the
                // one we just switched to. The caller will be fetching our output
                // using values().listen, and values() throws away all firings except
                // for the last one. Therefore, anything from the old input behaviour
                // that might have happened during this transaction will be suppressed.
                killInner();
                const behavior_& ba = *pa.cast_ptr<behavior_>(NULL);
                *pKillInner = optional<function<void()>>(
                    ba.values_().listen_raw_(trans, target, push, false)
                );
            }, false);
            return std::get<0>(p).add_cleanup_(killInner).add_cleanup_(killOuter).hold_(za);
        }

    };  // end namespace impl
};  // end namespace sodium
