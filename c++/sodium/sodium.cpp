/**
 * Copyright (c) 2012-2013, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#include <sodium/sodium.h>

using namespace std;
using namespace boost;


namespace sodium {

    namespace impl {

        event_::event_()
        {
        }

        /*!
         * listen to events.
         */
        std::function<void()>* event_::listen_raw(
                    transaction_impl* trans0,
                    const std::shared_ptr<impl::node>& target,
                    std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handle,
                    bool suppressEarlierFirings) const
        {
            {
                vector<light_ptr> items;
                sample_now(items);
                for (auto it = items.begin(); it != items.end(); ++it)
                    if (handle)
                        (*handle)(target, trans0, *it);
                    else
                        send(target, trans0, *it);
            }
            return listen_impl(trans0, target, handle, suppressEarlierFirings);
        }

        behavior_ event_::hold_(transaction_impl* trans, const light_ptr& initA) const
        {
            return behavior_(
                std::shared_ptr<impl::behavior_impl>(impl::hold(trans, initA, *this))
            );
        }

        #define KILL_ONCE(ppKill) \
            do { \
                function<void()>* pKill = *ppKill; \
                if (pKill != NULL) { \
                    *ppKill = NULL; \
                    (*pKill)(); \
                    delete pKill; \
                } \
            } while (0)

        event_ event_::once_(transaction_impl* trans) const
        {
            std::shared_ptr<function<void()>*> ppKill(new function<void()>*(NULL));

            auto p_sample_now(this->p_sample_now);
            auto p = impl::unsafe_new_event(new event_::sample_now_func([ppKill, p_sample_now] (vector<light_ptr>& items) {
                size_t start = items.size();
                if (p_sample_now)
                    (*p_sample_now)(items);
                if (items.begin() + start != items.end()) {
                    auto it = items.begin();
                    ++it;
                    items.erase(it, items.end());
                    KILL_ONCE(ppKill);
                }
            }));
            *ppKill = listen_raw(trans, std::get<1>(p),
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [ppKill] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                        send(target, trans, ptr);
                        KILL_ONCE(ppKill);
                    }), false);
            return std::get<0>(p).unsafe_add_cleanup(new std::function<void()>([ppKill] () {
                KILL_ONCE(ppKill);
            }));
        }

        event_ event_::merge_(transaction_impl* trans, const event_& other) const {
            auto sample_now_1(this->p_sample_now);
            auto sample_now_2(other.p_sample_now);
            auto p = impl::unsafe_new_event(sample_now_1 || sample_now_2 ? new event_::sample_now_func([sample_now_1, sample_now_2] (std::vector<light_ptr>& items) {
                if (sample_now_1)
                    (*sample_now_1)(items);
                if (sample_now_2)
                    (*sample_now_2)(items);
            }) : NULL);
            auto target = std::get<1>(p);
            auto kill_one = this->listen_raw(trans, target, NULL, false);
            auto kill_two = other.listen_raw(trans, target, NULL, false);
            return std::get<0>(p).unsafe_add_cleanup(kill_one, kill_two);
        }

        struct coalesce_state {
            coalesce_state() {}
            ~coalesce_state() {}
            boost::optional<light_ptr> oValue;
        };

        event_ event_::coalesce_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine) const
        {
            std::shared_ptr<coalesce_state> pState(new coalesce_state);
            auto p_sample_now(this->p_sample_now);
            auto p = impl::unsafe_new_event(new event_::sample_now_func([p_sample_now, combine] (vector<light_ptr>& items) {
                size_t start = items.size();
                if (p_sample_now)
                    (*p_sample_now)(items);
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
            }));
            auto kill = listen_raw(trans, std::get<1>(p),
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [pState, combine] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                        if (!pState->oValue) {
                            pState->oValue = boost::optional<light_ptr>(ptr);
                            trans->prioritized(target, [target, pState] (transaction_impl* trans) {
                                if (pState->oValue) {
                                    send(target, trans, pState->oValue.get());
                                    pState->oValue = boost::optional<light_ptr>();
                                }
                            });
                        }
                        else
                            pState->oValue = make_optional(combine(pState->oValue.get(), ptr));
                    }), false);
            return std::get<0>(p).unsafe_add_cleanup(kill);
        }

        event_ event_::last_firing_only_(transaction_impl* trans) const
        {
            return coalesce_(trans, [] (const light_ptr& fst, const light_ptr& snd) {
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
            auto p = impl::unsafe_new_event(new event_::sample_now_func([me, combine, beh] (vector<light_ptr>& items) {
                size_t start = items.size();
                me.sample_now(items);
                for (auto it = items.begin() + start; it != items.end(); ++it)
                    *it = combine(*it, beh.impl->sample());
            }));
            auto kill = listen_raw(trans, std::get<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                        [beh, combine] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& a) {
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
            auto p_sample_now(this->p_sample_now);
            auto p = impl::unsafe_new_event(
                p_sample_now != NULL
                ? new event_::sample_now_func([p_sample_now, pred] (vector<light_ptr>& output) {
                    vector<light_ptr> input;
                    (*p_sample_now)(input);
                    for (auto it = input.begin(); it != input.end(); ++it)
                        if (pred(*it))
                            output.push_back(*it);
                })
                : NULL);
            auto kill = listen_raw(trans, std::get<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                        [pred] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            if (pred(ptr)) send(target, trans, ptr);
                        }), false);
            return std::get<0>(p).unsafe_add_cleanup(kill);
        }

        class holder {
            public:
                holder(
                    std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handler
                ) : handler(handler) {}
                ~holder() {
                    delete handler;
                }
                inline void handle(const std::shared_ptr<node>& target, transaction_impl* trans, const light_ptr& value) const
                {
                    if (handler)
                        (*handler)(target, trans, value);
                    else
                        send(target, trans, value);
                }

            private:
                std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handler;
        };

        behavior_impl::behavior_impl(const light_ptr& constant)
            : changes(event_()),
              sample([constant] () { return constant; }),
              kill(NULL)
        {
        }

        behavior_impl::behavior_impl(
            const event_& changes,
            const std::function<light_ptr()>& sample,
            std::function<void()>* kill,
            const std::shared_ptr<behavior_impl>& parent)
            : changes(changes), sample(sample), kill(kill), parent(parent)
        {
        }

        behavior_impl::~behavior_impl()
        {
            if (kill) {
                (*kill)();
                delete kill;
            }
        }

        /*!
         * Function to push a value into an event
         */
        void send(const std::shared_ptr<node>& n, transaction_impl* trans, const light_ptr& ptr)
        {
            int ifs = 0;
            node::target* fs[16];
            std::list<node::target*> fsOverflow;
            {
                if (n->firings.begin() == n->firings.end())
                    trans->last([n] () {
                        n->firings.clear();
                    });
                n->firings.push_front(ptr);
                auto it = n->targets.begin();
                while (it != n->targets.end()) {
                    fs[ifs++] = &*it;
                    it++;
                    if (ifs == 16) {
                        while (it != n->targets.end()) {
                            fsOverflow.push_back(&*it);
                            it++;
                        }
                        break;
                    }
                }
            }
            for (int i = 0; i < ifs; i++)
                ((holder*)fs[i]->h)->handle(fs[i]->n, trans, ptr);
            for (auto it = fsOverflow.begin(); it != fsOverflow.end(); ++it)
                ((holder*)(*it)->h)->handle((*it)->n, trans, ptr);
        }

        /*!
         * Creates an event, that values can be pushed into using impl::send(). 
         */
        std::tuple<event_, std::shared_ptr<node>> unsafe_new_event(
            event_::sample_now_func* sample_now)
        {
            std::shared_ptr<node> n(new node);
            std::weak_ptr<node> n_weak(n);
            n->listen_impl = boost::intrusive_ptr<listen_impl_func<H_NODE>>(
                new listen_impl_func<H_NODE>(new listen_impl_func<H_NODE>::closure([n_weak] (transaction_impl* trans,
                        const std::shared_ptr<node>& target,
                        std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handler,
                        bool suppressEarlierFirings) -> std::function<void()>* {  // Register listener
                    std::shared_ptr<node> n = n_weak.lock();
                    if (n) {
                        std::forward_list<light_ptr> firings;
                        holder* h = new holder(handler);
                        {
                            trans->part->mx.lock();
                            n->link(h, target);
                            trans->part->mx.unlock();
                            firings = n->firings;
                        }
                        if (!suppressEarlierFirings && firings.begin() != firings.end())
                            for (auto it = firings.begin(); it != firings.end(); it++)
                                h->handle(target, trans, *it);
                        partition* part = trans->part;
                        return new std::function<void()>([part, n_weak, h] () {  // Unregister listener
                            std::shared_ptr<node> n = n_weak.lock();
                            if (n) {
                                part->mx.lock();
                                n->unlink(h);
                                part->mx.unlock();
                            }
                            delete h;
                        });
                    }
                    else {
                        delete handler;
                        return NULL;
                    }
                }))
            );
            boost::intrusive_ptr<listen_impl_func<H_EVENT>> li_event(
                reinterpret_cast<listen_impl_func<H_EVENT>*>(n->listen_impl.get()));
            return std::make_tuple(event_(li_event, sample_now), n);
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
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            if (input.is_never())
                return new behavior_impl(initValue);
            else {
#endif
                std::shared_ptr<behavior_state> state(new behavior_state(initValue));
                auto kill = input.listen_raw(trans0, std::shared_ptr<node>(new node(SODIUM_IMPL_RANK_T_MAX)),
                    new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                        [state] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& ptr) {
                            bool first = !state->update;
                            state->update = boost::optional<light_ptr>(ptr);
                            if (first)
                                trans->last([state] () {
                                    state->current = state->update.get();
                                    state->update = boost::optional<light_ptr>();
                                });
                            send(target, trans, ptr);
                        }), false);
                auto sample = [state] () {
                    return state->current;
                };
                return new behavior_impl(input, sample, kill, std::shared_ptr<behavior_impl>());
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

        event_ behavior_::values_(transaction_impl* trans) const
        {
            auto sample = impl->sample;
            return event_(
                changes_().p_listen_impl,
                new event_::sample_now_func([sample] (vector<light_ptr>& items) { items.push_back(sample()); })
            ).last_firing_only_(trans);
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
                return impl::map_(trans0, f, ba);  // map optimizes to a constant where ba is constant
            }
            else {
                boost::optional<light_ptr> oca = ba.get_constant_value();
                if (oca) {  // 'a' value is constant but function is not
                    auto a = oca.get();
                    return impl::map_(trans0, [a] (const light_ptr& pf) -> light_ptr {
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
                    auto kill1 = bf.values_(trans0).listen_raw(trans0, target,
                            new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                                [state] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& pf) {
                                    state->oF = boost::make_optional(*pf.cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL));
                                    auto oo = state->calcB();
                                    if (oo)
                                        send(target, trans, oo.get());
                                }), false);
                    auto kill2 = ba.values_(trans0).listen_raw(trans0, target,
                            new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                                [state] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& pa) {
                                    state->oA = boost::make_optional(pa);
                                    auto oo = state->calcB();
                                    if (oo)
                                        send(target, trans, oo.get());
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

        event_ event_::add_cleanup_(transaction_impl* trans, std::function<void()>* cleanup) const
        {
            auto p = impl::unsafe_new_event(
                p_sample_now != NULL
                ? new event_::sample_now_func(*p_sample_now)
                : NULL);
            auto kill = listen_raw(trans, std::get<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(send),
                    false);
            return std::get<0>(p).unsafe_add_cleanup(kill, cleanup);
        }

        /*!
         * Map a function over this event to modify the output value.
         */
        event_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f, const event_& ev)
        {
            auto p_sample_now(ev.p_sample_now);
            auto p = impl::unsafe_new_event(
                p_sample_now != NULL
                ? new event_::sample_now_func([p_sample_now, f] (vector<light_ptr>& items) {
                    size_t start = items.size();
                    (*p_sample_now)(items);
                    for (auto it = items.begin() + start; it != items.end(); ++it)
                        *it = f(*it);
                })
                : NULL);
            auto kill = ev.listen_raw(trans, std::get<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                        [f] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                            send(target, trans, f(ptr));
                        }), false);
            return std::get<0>(p).unsafe_add_cleanup(kill);
        }

        behavior_ map_(transaction_impl* trans, const std::function<light_ptr(const light_ptr&)>& f, const behavior_& beh) {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            auto ca = beh.get_constant_value();
            if (ca)
                return behavior_(f(ca.get()));
            else
#endif
                return behavior_(
                    new behavior_impl(
                        map_(trans, f, underlying_event(beh)),
                        [f, beh] () -> light_ptr {
                            return f(beh.impl->sample());
                        },
                        NULL,
                        beh.impl
                    )
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
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [pKillInner] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans1, const light_ptr& pea) {
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
            auto killOuter = bba.values_(trans0).listen_raw(trans0, std::get<1>(p),
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [pKillInner] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& pa) {
                        // Note: If any switch takes place during a transaction, then the
                        // values().listen will always cause a sample to be fetched from the
                        // one we just switched to. The caller will be fetching our output
                        // using values().listen, and values() throws away all firings except
                        // for the last one. Therefore, anything from the old input behaviour
                        // that might have happened during this transaction will be suppressed.
                        KILL_ONCE(pKillInner);
                        const behavior_& ba = *pa.cast_ptr<behavior_>(NULL);
                        *pKillInner = ba.values_(trans).listen_raw(trans, target, NULL, false);
                    }), false);
            return std::get<0>(p).unsafe_add_cleanup(new std::function<void()>([pKillInner] {
                KILL_ONCE(pKillInner);
            }), killOuter).hold_(trans0, za);
        }

    };  // end namespace impl
};  // end namespace sodium
