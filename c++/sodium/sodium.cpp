/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#include <sodium/sodium.h>

using namespace std;
using namespace boost;


void intrusive_ptr_add_ref(sodium::impl::event_impl* p)
{
    ++p->ref_count;
}

void intrusive_ptr_release(sodium::impl::event_impl* p)
{
    if (--p->ref_count == 0)
        delete p;
}

namespace sodium {
#define GCC_VERSION (__GNUC__ * 10000 \
                   + __GNUC_MINOR__ * 100 \
                   + __GNUC_PATCHLEVEL__)

#if GCC_VERSION < 40700
#define WORKAROUND_GCC_46_BUG
#endif

    namespace impl {

        void event_impl::touch() const
        {
        }

        event_::event_()
            : impl(new event_impl(NULL, NULL))
        {
        }

        /*!
         * listen to events.
         */
        std::function<void()>* event_::listen_raw(
                    transaction_impl* trans0,
                    const std::shared_ptr<impl::node>& target,
                    std::function<void(transaction_impl*, const light_ptr&)>* handle,
                    bool suppressEarlierFirings) const
        {
            {
                vector<light_ptr> items;
                sample_now(items);
                for (auto it = items.begin(); it != items.end(); ++it)
                    if (handle)
                        (*handle)(trans0, *it);
                    else
                        send(target, trans0, *it);
            }
            return listen_impl(trans0, target, handle, suppressEarlierFirings, impl);
        }

        behavior_ event_::hold_(transaction_impl* trans, const light_ptr& initA) const
        {
            return behavior_(
                std::shared_ptr<impl::behavior_impl>(impl::hold(trans, initA, *this))
            );
        }

        #define KILL_ONCE(pKillInner) \
            do { \
                if (*pKillInner != NULL) { \
                    (**pKillInner)(); \
                    delete *pKillInner; \
                    *pKillInner = NULL; \
                } \
            } while (0)

        event_ event_::once_(transaction_impl* trans) const
        {
            std::shared_ptr<function<void()>*> ppKill(new function<void()>*(NULL));

            event_ me(*this);
            auto p = impl::unsafe_new_event(new event_impl::sample_now_func([ppKill, me] (vector<light_ptr>& items) {
                size_t start = items.size();
                me.sample_now(items);
                if (items.begin() + start != items.end()) {
                    auto it = items.begin();
                    ++it;
                    items.erase(it, items.end());
                    KILL_ONCE(ppKill);
                }
            }));
            auto target = std::get<1>(p);
            *ppKill = listen_raw(trans, target,
                new std::function<void(transaction_impl*, const light_ptr&)>(
                    [target, ppKill] (impl::transaction_impl* trans, const light_ptr& ptr) {
                    send(target, trans, ptr);
                    KILL_ONCE(ppKill);
                }), false);
            return std::get<0>(p).unsafe_add_cleanup(new std::function<void()>([ppKill] () {
                KILL_ONCE(ppKill);
            }));
        }

        event_ event_::merge_(transaction_impl* trans, const event_& other) const {
            const event_& me(*this);
            auto p = impl::unsafe_new_event(new event_impl::sample_now_func([me, other] (std::vector<light_ptr>& items) {
                me.sample_now(items);
                other.sample_now(items);
            }));
            auto target = std::get<1>(p);
            auto kill_one = this->listen_raw(trans, target, NULL, false);
            auto kill_two = other.listen_raw(trans, target, NULL, false);
            return std::get<0>(p).unsafe_add_cleanup(kill_one, kill_two);
        }

        struct coalesce_state {
            coalesce_state(
                const std::shared_ptr<node>& target,
                std::function<void(transaction_impl*, const light_ptr&)>* handle)
            : target(target), handle(handle) {}
            ~coalesce_state() { delete handle; }
            boost::optional<light_ptr> oValue;
            std::shared_ptr<node> target;
            std::function<void(transaction_impl*, const light_ptr&)>* handle;
        };

        event_ event_::coalesce_(const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const
        {
            const event_& me(*this);
            return event_(
                new event_impl(
                    me.impl->listen_impl != NULL
                    ? new event_impl::listen_impl_func(
                        [combine, me] (transaction_impl* trans, const std::shared_ptr<node>& target,
                                        std::function<void(transaction_impl*, const light_ptr&)>* handle,
                                        bool suppressEarlierFirings,
                                        const boost::intrusive_ptr<event_impl>& impl)
                                                                                    -> std::function<void()>* {
                            std::shared_ptr<coalesce_state> pState(new coalesce_state(target, handle));
                            return me.listen_impl(
                                trans, target,
                                new std::function<void(transaction_impl*, const light_ptr&)>(
                                    [combine, pState] (transaction_impl* trans, const light_ptr& ptr) {
                                        if (!pState->oValue) {
                                            pState->oValue = boost::optional<light_ptr>(ptr);
                                            trans->prioritized(pState->target, [pState] (transaction_impl* trans) {
                                                if (pState->oValue) {
                                                    if (pState->handle)
                                                        (*pState->handle)(trans, pState->oValue.get());
                                                    else
                                                        send(pState->target, trans, pState->oValue.get());
                                                    pState->oValue = boost::optional<light_ptr>();
                                                }
                                            });
                                        }
                                        else
                                            pState->oValue = make_optional(combine(pState->oValue.get(), ptr));
                                    }),
                                suppressEarlierFirings,
                                impl);
                        })
                    : NULL,
                    impl->sample_now != NULL
                    ? new event_impl::sample_now_func(
                        [me, combine] (vector<light_ptr>& items) {
                            size_t start = items.size();
                            me.sample_now(items);
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
                        })
                    : NULL,
                    impl
                )
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
        event_ event_::snapshot_(transaction_impl* trans, const behavior_& beh, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const
        {
            const event_& me(*this);
            auto p = impl::unsafe_new_event(new event_impl::sample_now_func([me, combine, beh] (vector<light_ptr>& items) {
                size_t start = items.size();
                me.sample_now(items);
                for (auto it = items.begin() + start; it != items.end(); ++it)
                    *it = combine(*it, beh.impl->sample());
            }));
            auto target = std::get<1>(p);
            auto kill = listen_raw(trans, target,
                    new std::function<void(transaction_impl*, const light_ptr&)>(
                        [beh, target, combine] (impl::transaction_impl* trans, const light_ptr& a) {
                        send(target, trans, combine(a, beh.impl->sample()));
                    }), false);
            return std::get<0>(p).unsafe_add_cleanup(kill);
        }

        /*!
         * Filter this event based on the specified predicate, passing through values
         * where the predicate returns true.
         */
        event_ event_::filter_(transaction_impl* trans, const std::function<bool(const light_ptr&)>& pred) const
        {
            const event_& me(*this);
            auto p = impl::unsafe_new_event(
                impl->sample_now != NULL
                ? new event_impl::sample_now_func([me, pred] (vector<light_ptr>& output) {
                    vector<light_ptr> input;
                    me.sample_now(input);
                    for (auto it = input.begin(); it != input.end(); ++it)
                        if (pred(*it))
                            output.push_back(*it);
                })
                : NULL);
            auto target = std::get<1>(p);
            auto kill = listen_raw(trans, target,
                    new std::function<void(transaction_impl*, const light_ptr&)>(
                        [pred, target] (impl::transaction_impl* trans, const light_ptr& ptr) {
                            if (pred(ptr)) send(target, trans, ptr);
                        }), false);
            return std::get<0>(p).unsafe_add_cleanup(kill);
        }

        class holder {
            public:
                holder(
                    const std::shared_ptr<impl::node>& target,
                    std::function<void(transaction_impl*, const light_ptr&)>* handler,
                    const boost::intrusive_ptr<event_impl>& impl
                ) : target(target), handler(handler), impl(impl) {}
                ~holder() {
                    delete handler;
                }
                inline void handle(transaction_impl* trans, const light_ptr& value) const
                {
                    if (handler)
                        (*handler)(trans, value);
                    else
                        send(target, trans, value);
                }
            private:
                std::shared_ptr<impl::node> target;
                std::function<void(transaction_impl*, const light_ptr&)>* handler;
                boost::intrusive_ptr<event_impl> impl;  // keeps the event we're listening to alive
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

#if defined(WORKAROUND_GCC_46_BUG)
        struct new_event_listener {
            new_event_listener(const std::shared_ptr<node>& n) : n(n) {}
            std::shared_ptr<node> n;
            std::function<void()> operator () (
                transaction_impl* trans,
                const std::shared_ptr<impl::node>& target,
                std::function<void(transaction_impl*, const light_ptr&)>* handle,
                bool suppressEarlierFirings,
                const boost::intrusive_ptr<event_impl>& cleanerUpper
            ) {
                std::list<light_ptr> firings;
                holder* h = new holder(target, handle, cleanerUpper);
                {
                    trans->part->mx.lock();
                    n->link(h, target);
                    trans->part->mx.unlock();
                    firings = n->firings;
                }
                if (!suppressEarlierFirings && firings.begin() != firings.end())
                    for (auto it = firings.begin(); it != firings.end(); it++)
                        h->handle(trans, *it);
                partition* part = trans->part;
                return [part, n, h] () {  // Unregister listener
                    part->mx.lock();
                    if (n->unlink(h)) {
                        part->mx.unlock();
                        delete h;
                    }
                    else
                        part->mx.unlock();
                };
            }
        };
#endif

        /*!
         * Function to push a value into an event
         */
        void send(const std::shared_ptr<node>& n, transaction_impl* trans, const light_ptr& ptr)
        {
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
        }

        /*!
         * Creates an event, that values can be pushed into using impl::send(). 
         */
        std::tuple<event_, std::shared_ptr<node>> unsafe_new_event(
            event_impl::sample_now_func* sample_now)
        {
            std::shared_ptr<node> n(new node);
            boost::intrusive_ptr<event_impl> impl(
                new event_impl(
#if defined(WORKAROUND_GCC_46_BUG)
                    new event_impl::listen_impl_func(new_event_listener(n)),
#else
                    new event_impl::listen_impl_func([n] (transaction_impl* trans,
                            const std::shared_ptr<node>& target,
                            std::function<void(transaction_impl*, const light_ptr&)>* handler,
                            bool suppressEarlierFirings,
                            const boost::intrusive_ptr<event_impl>& impl) {  // Register listener
                        std::list<light_ptr> firings;
                        holder* h = new holder(target, handler, impl);
                        {
                            trans->part->mx.lock();
                            n->link(h, target);
                            trans->part->mx.unlock();
                            firings = n->firings;
                        }
                        if (!suppressEarlierFirings && firings.begin() != firings.end())
                            for (auto it = firings.begin(); it != firings.end(); it++)
                                h->handle(trans, *it);
                        partition* part = trans->part;
                        return new std::function<void()>([part, n, h] () {  // Unregister listener
                            part->mx.lock();
                            if (n->unlink(h)) {
                                part->mx.unlock();
                                delete h;
                            }
                            else
                                part->mx.unlock();
                        });
                    }),
#endif
                    sample_now
                )
            );
            //n->impl = impl;
            return std::make_tuple(
                // The event
                event_(impl, false),
                n
            );
        }

        event_sink_impl::event_sink_impl()
        {
        }

        event_ event_sink_impl::construct()
        {
            auto p = impl::unsafe_new_event();
            this->target = std::get<1>(p);
            return std::get<0>(p);
        }

        void event_sink_impl::send(transaction_impl* trans, const light_ptr& ptr) const
        {
            auto target(this->target);
            trans->prioritized(target, [target, ptr] (transaction_impl* trans_impl) {
                sodium::impl::send(target, trans_impl, ptr);
            });
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
            auto target = std::get<1>(p);
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            if (input.is_never())
                return new behavior_impl(input, [initValue] () { return initValue; });
            else {
#endif
                std::shared_ptr<behavior_state> state(new behavior_state(initValue));
                auto unlisten = input.listen_raw(trans0, target,
                    new std::function<void(transaction_impl*, const light_ptr&)>(
                        [target, state] (transaction_impl* trans, const light_ptr& ptr) {
                            bool first = !state->update;
                            state->update = boost::optional<light_ptr>(ptr);
                            if (first)
                                trans->last([state] () {
                                    state->current = state->update.get();
                                    state->update = boost::optional<light_ptr>();
                                });
                            send(target, trans, ptr);
                        }), false);
                auto changes = std::get<0>(p).unsafe_add_cleanup(unlisten);
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
                new event_impl(
                    changes_().impl->listen_impl != NULL
                    ? new event_impl::listen_impl_func(*changes_().impl->listen_impl)
                    : NULL,
                    new event_impl::sample_now_func([sample] (vector<light_ptr>& items) { items.push_back(sample()); }),
                    changes_().impl
                )
            ).last_firing_only_();
        }

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
        /*!
         * For optimization, if this behavior is a constant, then return its value.
         */
        boost::optional<light_ptr> behavior_::get_constant_value() const
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
            boost::optional<light_ptr> ocf = bf.get_constant_value();
            if (ocf) { // function is constant
                auto f = *ocf.get().cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
                return impl::map_(f, ba);  // map optimizes to a constant where ba is constant
            }
            else {
                boost::optional<light_ptr> oca = ba.get_constant_value();
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
                    auto target = std::get<1>(p);
                    auto update = [state, target] (transaction_impl* trans) {
                        auto oo = state->calcB();
                        if (oo)
                            send(target, trans, oo.get());
                    };
                    auto kill1 = bf.values_().listen_raw(trans0, target,
                            new std::function<void(transaction_impl*, const light_ptr&)>(
                                [state, update] (transaction_impl* trans, const light_ptr& pf) {
                                    state->oF = boost::make_optional(*pf.cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL));
                                    update(trans);
                                }), false);
                    auto kill2 = ba.values_().listen_raw(trans0, target,
                            new std::function<void(transaction_impl*, const light_ptr&)>(
                                [state, update] (transaction_impl* trans, const light_ptr& pa) {
                                    state->oA = boost::make_optional(pa);
                                    update(trans);
                                }), false);
                    auto f = *bf.impl->sample().cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
                    return behavior_(impl::hold(
                        trans0,
                        f(ba.impl->sample()),
                        std::get<0>(p).unsafe_add_cleanup(kill1, kill2)
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
            return event_(
                new event_impl(
                    ev.impl->listen_impl != NULL
                    ? new event_impl::listen_impl_func(
                        [f,ev] (transaction_impl* trans0,
                                std::shared_ptr<node> target,
                                std::function<void(transaction_impl*, const light_ptr&)>* handle,
                                bool suppressEarlierFirings,
                                const boost::intrusive_ptr<event_impl>& cu) {
                            std::shared_ptr<std::function<void(transaction_impl*, const light_ptr&)>> pHandle(handle);
                            return ev.listen_impl(trans0, target,
                                new std::function<void(transaction_impl*, const light_ptr&)>(
                                    [pHandle, target, f] (transaction_impl* trans, const light_ptr& ptr) {
                                        if (pHandle.get())
                                            (*pHandle)(trans, f(ptr));
                                        else
                                            send(target, trans, f(ptr));
                                    }), suppressEarlierFirings, cu);
                        }
                    )
                    : NULL,
                    ev.impl->sample_now != NULL
                    ? new event_impl::sample_now_func([ev, f] (vector<light_ptr>& items) {
                        size_t start = items.size();
                        ev.sample_now(items);
                        for (auto it = items.begin() + start; it != items.end(); ++it)
                            *it = f(*it);
                    })
                    : NULL,
                    ev.impl
                )
            );
        }

        behavior_ map_(const std::function<light_ptr(const light_ptr&)>& f, const behavior_& beh) {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            auto ca = beh.get_constant_value();
            if (ca)
                return behavior_(f(ca.get()));
            else
#endif
                return behavior_(
                    map_(f, underlying_event(beh)),
                    [f, beh] () -> light_ptr {
                        return f(beh.impl->sample());
                    }
                );
        }

        event_ switch_e(transaction_impl* trans0, const behavior_& bea)
        {
            auto p = unsafe_new_event();
            auto target = std::get<1>(p);
            std::shared_ptr<function<void()>*> pKillInner(new function<void()>*(
                bea.impl->sample().cast_ptr<event_>(NULL)->listen_raw(trans0, target, NULL, false)
            ));

            auto killOuter = bea.changes_().listen_raw(trans0, target,
                new std::function<void(transaction_impl*, const light_ptr&)>(
                    [pKillInner, target] (impl::transaction_impl* trans1, const light_ptr& pea) {
                        const event_& ea = *pea.cast_ptr<event_>(NULL);
                        trans1->last([pKillInner, ea, trans1, target] () {
                            KILL_ONCE(pKillInner);
                            *pKillInner = ea.listen_raw(trans1, target, NULL, true);
                        });
                    }),
                false
            );
            return std::get<0>(p).unsafe_add_cleanup(new std::function<void()>([pKillInner] {
                KILL_ONCE(pKillInner);
            }), killOuter);
        }

        behavior_ switch_b(transaction_impl* trans0, const behavior_& bba)
        {
            light_ptr za = bba.impl->sample().cast_ptr<behavior_>(NULL)->impl->sample();
            std::shared_ptr<function<void()>*> pKillInner(new function<void()>*(NULL));
            auto p = unsafe_new_event();
            auto target = std::get<1>(p);
            auto killOuter = bba.values_().listen_raw(trans0, target,
                new std::function<void(transaction_impl*, const light_ptr&)>(
                    [pKillInner, target] (transaction_impl* trans, const light_ptr& pa) {
                        // Note: If any switch takes place during a transaction, then the
                        // values().listen will always cause a sample to be fetched from the
                        // one we just switched to. The caller will be fetching our output
                        // using values().listen, and values() throws away all firings except
                        // for the last one. Therefore, anything from the old input behaviour
                        // that might have happened during this transaction will be suppressed.
                        KILL_ONCE(pKillInner);
                        const behavior_& ba = *pa.cast_ptr<behavior_>(NULL);
                        *pKillInner = ba.values_().listen_raw(trans, target, NULL, false);
                    }), false);
            return std::get<0>(p).unsafe_add_cleanup(new std::function<void()>([pKillInner] {
                KILL_ONCE(pKillInner);
            }), killOuter).hold_(trans0, za);
        }

    };  // end namespace impl
};  // end namespace sodium
