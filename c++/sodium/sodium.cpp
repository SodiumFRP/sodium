/**
 * Copyright (c) 2012-2014, Stephen Blackheath and Anthony Jones
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
#if defined(SODIUM_NO_CXX11)
        lambda0<void>* event_::listen_raw(
#else
        std::function<void()>* event_::listen_raw(
#endif
                    transaction_impl* trans,
                    const SODIUM_SHARED_PTR<impl::node>& target,
#if defined(SODIUM_NO_CXX11)
                    lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>* handler,
#else
                    std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handler,
#endif
                    bool suppressEarlierFirings,
                    bool pushSampleNows) const
        {
            SODIUM_SHARED_PTR<holder> h(new holder(handler));
            if (pushSampleNows && p_sample_now) {
                auto p_sample_now(this->p_sample_now);
                trans->prioritized(target, [target, h, p_sample_now] (transaction_impl* trans) {
                    {
                        vector<light_ptr> items;
                        (*p_sample_now)(items);
                        for (vector<light_ptr>::iterator it = items.begin(); it != items.end(); ++it)
                            h->handle(target, trans, *it);
                    }
                });
            }
            return listen_impl(trans, target, h, suppressEarlierFirings);
        }

        behavior_ event_::hold_(transaction_impl* trans, const light_ptr& initA) const
        {
            return behavior_(
                SODIUM_SHARED_PTR<impl::behavior_impl>(impl::hold(trans, initA, *this))
            );
        }

        behavior_ event_::hold_lazy_(transaction_impl* trans, const std::function<light_ptr()>& initA) const
        {
            return behavior_(
                SODIUM_SHARED_PTR<impl::behavior_impl>(impl::hold_lazy(trans, initA, *this))
            );
        }

#if defined(SODIUM_NO_CXX11)
        #define KILL_ONCE(ppKill) \
            do { \
                lambda0<void>* pKill = *ppKill; \
                if (pKill != NULL) { \
                    *ppKill = NULL; \
                    (*pKill)(); \
                    delete pKill; \
                } \
            } while (0)

        struct once_handler : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            once_handler(const SODIUM_SHARED_PTR<lambda0<void>*>& ppKill) : ppKill(ppKill) {}
            SODIUM_SHARED_PTR<lambda0<void>*> ppKill;

            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, transaction_impl* trans, const light_ptr& ptr) const {
                send(target, trans, ptr);
                KILL_ONCE(ppKill);
            }
        };
        struct once_killer : i_lambda0<void> {
            once_killer(const SODIUM_SHARED_PTR<lambda0<void>*>& ppKill) : ppKill(ppKill) {}
            SODIUM_SHARED_PTR<lambda0<void>*> ppKill;

            virtual void operator () () const {
                KILL_ONCE(ppKill);
            }
        };
        struct once_sample_now : i_lambda1<void, vector<light_ptr>&> {
            once_sample_now(const SODIUM_SHARED_PTR<lambda0<void>*>& ppKill, const SODIUM_SHARED_PTR<event_::sample_now_func>& p_sample_now)
            : ppKill(ppKill), p_sample_now(p_sample_now) {}
            SODIUM_SHARED_PTR<lambda0<void>*> ppKill;
            SODIUM_SHARED_PTR<event_::sample_now_func> p_sample_now;
            virtual void operator () (vector<light_ptr>& items) const {
                size_t start = items.size();
                if (p_sample_now)
                    (*p_sample_now)(items);
                if (items.begin() + start != items.end()) {
                    vector<light_ptr>::iterator it = items.begin();
                    ++it;
                    items.erase(it, items.end());
                    KILL_ONCE(ppKill);
                }
            }
        };
#else
        #define KILL_ONCE(ppKill) \
            do { \
                function<void()>* pKill = *ppKill; \
                if (pKill != NULL) { \
                    *ppKill = NULL; \
                    (*pKill)(); \
                    delete pKill; \
                } \
            } while (0)
#endif

        event_ event_::once_(transaction_impl* trans) const
        {
#if defined(SODIUM_NO_CXX11)
            SODIUM_SHARED_PTR<lambda0<void>*> ppKill(new lambda0<void>*(NULL));
#else
            SODIUM_SHARED_PTR<function<void()>*> ppKill(new function<void()>*(NULL));
#endif

            SODIUM_SHARED_PTR<sample_now_func> p_sample_now(this->p_sample_now);
#if defined(SODIUM_NO_CXX11)
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event(new event_::sample_now_func(
                new once_sample_now(ppKill, p_sample_now)
            ));
#else
            auto p = impl::unsafe_new_event(new event_::sample_now_func([ppKill, p_sample_now] (vector<light_ptr>& items) {
                size_t start = items.size();
                if (p_sample_now)
                    (*p_sample_now)(items);
                if (items.begin() + start != items.end()) {
                    vector<light_ptr>::iterator it = items.begin();
                    ++it;
                    items.erase(it, items.end());
                    KILL_ONCE(ppKill);
                }
            }));
#endif
            *ppKill = listen_raw(trans, SODIUM_TUPLE_GET<1>(p),
#if defined(SODIUM_NO_CXX11)
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new once_handler(ppKill)
                ),
#else
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [ppKill] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) {
                        send(target, trans, ptr);
                        KILL_ONCE(ppKill);
                    }),
#endif
                false, false);
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(
#if defined(SODIUM_NO_CXX11)
                new lambda0<void>(new once_killer(ppKill))
#else
                new std::function<void()>([ppKill] () {
                    KILL_ONCE(ppKill);
                })
#endif
            );
        }

#if defined(SODIUM_NO_CXX11)
        struct merge_sample_now : i_lambda1<void, std::vector<light_ptr>&> {
            merge_sample_now(const SODIUM_SHARED_PTR<event_::sample_now_func>& sample_now_1,
                             const SODIUM_SHARED_PTR<event_::sample_now_func>& sample_now_2)
            : sample_now_1(sample_now_1),
              sample_now_2(sample_now_2) {}
            SODIUM_SHARED_PTR<event_::sample_now_func> sample_now_1, sample_now_2;
            virtual void operator () (std::vector<light_ptr>& items) const {
                if (sample_now_1)
                    (*sample_now_1)(items);
                if (sample_now_2)
                    (*sample_now_2)(items);
            }
        };
        struct send_task : i_lambda1<void, transaction_impl*> {
            send_task(const SODIUM_SHARED_PTR<impl::node>& target, const light_ptr& value)
            : target(target), value(value) {}
            SODIUM_SHARED_PTR<impl::node> target;
            light_ptr value;
            virtual void operator () (transaction_impl* trans_impl) const {
                sodium::impl::send(target, trans_impl, value);
            }
        };
        struct merge_listen : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& a) const {
                trans->prioritized(target, new send_task(target, a));
            }
        };
#endif

        event_ event_::merge_(transaction_impl* trans, const event_& other) const {
#if defined(SODIUM_NO_CXX11)
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event(
                p_sample_now || other.p_sample_now ? new event_::sample_now_func(new merge_sample_now(p_sample_now, other.p_sample_now))
                                                   : NULL);
#else
            SODIUM_SHARED_PTR<sample_now_func> sample_now_1(this->p_sample_now);
            SODIUM_SHARED_PTR<sample_now_func> sample_now_2(other.p_sample_now);
            auto p = impl::unsafe_new_event(sample_now_1 || sample_now_2 ? new event_::sample_now_func([sample_now_1, sample_now_2] (std::vector<light_ptr>& items) {
                if (sample_now_1)
                    (*sample_now_1)(items);
                if (sample_now_2)
                    (*sample_now_2)(items);
            }) : NULL);
#endif
            SODIUM_SHARED_PTR<impl::node> left(new impl::node);
            const SODIUM_SHARED_PTR<impl::node>& right = SODIUM_TUPLE_GET<1>(p);
            if (left->link(NULL, right))
                trans->to_regen = true;
            // defer right side to make sure merge is left-biased
#if defined(SODIUM_NO_CXX11)
*** TO DO
#else
            auto kill_one = this->listen_raw(trans, left,
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [right] (const std::shared_ptr<impl::node>&, impl::transaction_impl* trans, const light_ptr& a) {
                        send(right, trans, a);
                    }), false, false);
            auto kill_two = other.listen_raw(trans, right, NULL, false, false);
#endif
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill_one, kill_two);
        }

        struct coalesce_state {
            coalesce_state() {}
            ~coalesce_state() {}
            boost::optional<light_ptr> oValue;
        };
#if defined(SODIUM_NO_CXX11)
        struct coalesce_sample_now : i_lambda1<void, std::vector<light_ptr>&> {
            coalesce_sample_now(const SODIUM_SHARED_PTR<event_::sample_now_func>& p_sample_now,
                                const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine)
            : p_sample_now(p_sample_now), combine(combine) {}
            SODIUM_SHARED_PTR<event_::sample_now_func> p_sample_now;
            lambda2<light_ptr, const light_ptr&, const light_ptr&> combine;
            virtual void operator () (std::vector<light_ptr>& items) const {
                size_t start = items.size();
                if (p_sample_now)
                    (*p_sample_now)(items);
                std::vector<light_ptr>::iterator first = items.begin() + start;
                if (first != items.end()) {
                    std::vector<light_ptr>::iterator it = first + 1;
                    if (it != items.end()) {
                        light_ptr sum = *first;
                        while (it != items.end())
                            sum = combine(sum, *it++);
                        items.erase(first, items.end());
                        items.push_back(sum);
                    }
                }
            }
        };
        struct coalesce_prioritized : i_lambda1<void, transaction_impl*> {
            coalesce_prioritized(const SODIUM_SHARED_PTR<impl::node>& target,
                                 const SODIUM_SHARED_PTR<coalesce_state>& pState)
            : target(target), pState(pState) {}
            SODIUM_SHARED_PTR<impl::node> target;
            SODIUM_SHARED_PTR<coalesce_state> pState;
            virtual void operator () (transaction_impl* trans) const {
                if (pState->oValue) {
                    send(target, trans, pState->oValue.get());
                    pState->oValue = boost::optional<light_ptr>();
                }
            }
        };
        struct coalesce_listen : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            coalesce_listen(
                const SODIUM_SHARED_PTR<coalesce_state>& pState,
                const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine)
            : pState(pState), combine(combine) {}
            SODIUM_SHARED_PTR<coalesce_state> pState;
            lambda2<light_ptr, const light_ptr&, const light_ptr&> combine;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) const {
                if (!pState->oValue) {
                    pState->oValue = boost::optional<light_ptr>(ptr);
                    trans->prioritized(target, new coalesce_prioritized(target, pState));
                }
                else
                    pState->oValue = make_optional(combine(pState->oValue.get(), ptr));
            }
        };
#endif

        event_ event_::coalesce_(transaction_impl* trans,
#if defined(SODIUM_NO_CXX11)
                const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine
#else
                const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine
#endif
            ) const
        {
            SODIUM_SHARED_PTR<coalesce_state> pState(new coalesce_state);
#if defined(SODIUM_NO_CXX11)
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p =
                impl::unsafe_new_event(new event_::sample_now_func(new coalesce_sample_now(p_sample_now, combine)));
            lambda0<void>* kill = listen_raw(trans, SODIUM_TUPLE_GET<1>(p),
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new coalesce_listen(pState, combine)
                ), false);
#else
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
            auto kill = listen_raw(trans, SODIUM_TUPLE_GET<1>(p),
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
                    }), false, false);
#endif
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill);
        }

        event_ event_::last_firing_only_(transaction_impl* trans) const
        {
#if defined(SODIUM_NO_CXX11)
            return coalesce_(trans, new snd_arg<light_ptr,light_ptr>);
#else
            return coalesce_(trans, [] (const light_ptr& fst, const light_ptr& snd) {
                return snd;
            });
#endif
        }

#if defined(SODIUM_NO_CXX11)
        struct snapshot_sample_now : i_lambda1<void, std::vector<light_ptr>&> {
            snapshot_sample_now(const event_& me,
                                const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine,
                                const behavior_& beh)
            : me(me), combine(combine), beh(beh) {}
            event_ me;
            lambda2<light_ptr, const light_ptr&, const light_ptr&> combine;
            behavior_ beh;
            virtual void operator () (std::vector<light_ptr>& items) const {
                size_t start = items.size();
                me.sample_now(items);
                for (std::vector<light_ptr>::iterator it = items.begin() + start; it != items.end(); ++it)
                    *it = combine(*it, beh.impl->sample());
            }
        };
        struct snapshot_listen : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            snapshot_listen(
                const behavior_& beh,
                const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine)
            : beh(beh), combine(combine) {}
            behavior_ beh;
            lambda2<light_ptr, const light_ptr&, const light_ptr&> combine;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& a) const {
                send(target, trans, combine(a, beh.impl->sample()));
            }
        };
#endif

        /*!
         * Sample the behavior's value as at the transaction before the
         * current one, i.e. no changes from the current transaction are
         * taken.
         */
        event_ event_::snapshot_(transaction_impl* trans, const behavior_& beh,
#if defined(SODIUM_NO_CXX11)
                const lambda2<light_ptr, const light_ptr&, const light_ptr&>& combine
#else
                const std::function<light_ptr(const light_ptr&, const light_ptr&)>& combine
#endif
            ) const
        {
#if defined(SODIUM_NO_CXX11)
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p =
                impl::unsafe_new_event(new event_::sample_now_func(new snapshot_sample_now(*this, combine, beh)));
            lambda0<void>* kill = listen_raw(trans, SODIUM_TUPLE_GET<1>(p),
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new snapshot_listen(beh, combine)
                ), false);
#else
            const event_& me(*this);
            auto p = impl::unsafe_new_event(new event_::sample_now_func([me, combine, beh] (vector<light_ptr>& items) {
                size_t start = items.size();
                me.sample_now(items);
                for (auto it = items.begin() + start; it != items.end(); ++it)
                    *it = combine(*it, beh.impl->sample());
            }));
            auto kill = listen_raw(trans, SODIUM_TUPLE_GET<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                        [beh, combine] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans, const light_ptr& a) {
                        send(target, trans, combine(a, beh.impl->sample()));
                    }), false, false);
#endif
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill);
        }

#if defined(SODIUM_NO_CXX11)
        struct filter_sample_now : i_lambda1<void, std::vector<light_ptr>&> {
            filter_sample_now(const SODIUM_SHARED_PTR<event_::sample_now_func>& p_sample_now,
                              const lambda1<bool, const light_ptr&>& pred)
            : p_sample_now(p_sample_now), pred(pred) {}
            SODIUM_SHARED_PTR<event_::sample_now_func> p_sample_now;
            lambda1<bool, const light_ptr&> pred;
            virtual void operator () (std::vector<light_ptr>& output) const {
                vector<light_ptr> input;
                (*p_sample_now)(input);
                for (std::vector<light_ptr>::iterator it = input.begin(); it != input.end(); ++it)
                    if (pred(*it))
                        output.push_back(*it);
            }
        };
        struct filter_listen : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            filter_listen(const lambda1<bool, const light_ptr&>& pred)
            : pred(pred) {}
            lambda1<bool, const light_ptr&> pred;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) const {
                if (pred(ptr)) send(target, trans, ptr);
            }
        };
#endif

        /*!
         * Filter this event based on the specified predicate, passing through values
         * where the predicate returns true.
         */
        event_ event_::filter_(transaction_impl* trans,
#if defined(SODIUM_NO_CXX11)
                const lambda1<bool, const light_ptr&>& pred
#else
                const std::function<bool(const light_ptr&)>& pred
#endif
            ) const
        {
#if defined(SODIUM_NO_CXX11)
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p =
                impl::unsafe_new_event(new event_::sample_now_func(new filter_sample_now(p_sample_now, pred)));
            lambda0<void>* kill = listen_raw(trans, SODIUM_TUPLE_GET<1>(p),
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new filter_listen(pred)
                ), false);
#else
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
                        }), false, false);
#endif
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill);
        }

#if defined(SODIUM_NO_CXX11)
        struct behavior_const_sample : i_lambda0<light_ptr> {
            behavior_const_sample(const light_ptr& a) : a(a) {}
            light_ptr a;
            light_ptr operator () () const { return a; }
        };
#endif

        behavior_impl::behavior_impl()
            : updates(event_()),
              kill(NULL)
        {
        }

        behavior_impl::behavior_impl(
            const event_& updates,
            const SODIUM_SHARED_PTR<behavior_impl>& parent)
            : updates(updates), kill(NULL), parent(parent)
        {
        }

        behavior_impl::~behavior_impl()
        {
            if (kill) {
                (*kill)();
                delete kill;
            }
        }
        
#if defined(SODIUM_NO_CXX11)
        struct clear_firings : i_lambda0<void> {
            clear_firings(const SODIUM_SHARED_PTR<node>& n) : n(n) {}
            SODIUM_SHARED_PTR<node> n;
            virtual void operator () () const {
                n->firings.clear();
            }
        };
#endif

        /*!
         * Function to push a value into an event
         */
        void send(const SODIUM_SHARED_PTR<node>& n, transaction_impl* trans, const light_ptr& a)
        {
            if (n->firings.begin() == n->firings.end())
#if defined(SODIUM_NO_CXX11)
                trans->last(new clear_firings(n));
#else
                trans->last([n] () {
                    n->firings.clear();
                });
#endif
            n->firings.push_front(a);
            SODIUM_FORWARD_LIST<node::target>::iterator it = n->targets.begin();
            while (it != n->targets.end()) {
                node::target* f = &*it;
                trans->prioritized(f->n, [f, a] (transaction_impl* trans) {
                    ((holder*)f->h)->handle(f->n, trans, a);
                });
                it++;
            }
        }

#if defined(SODIUM_NO_CXX11)
        struct unregister : i_lambda0<void> {
            unregister(partition* part,
                       const SODIUM_WEAK_PTR<node>& n_weak,
                       holder* h)
            : part(part), n_weak(n_weak), h(h) {}
            partition* part;
            SODIUM_WEAK_PTR<node> n_weak;
            holder* h;
            virtual void operator () () const {
                SODIUM_SHARED_PTR<node> n = n_weak.lock();
                if (n) {
#if !defined(SODIUM_SINGLE_THREADED)
                    part->mx.lock();
#endif
                    n->unlink(h);
#if !defined(SODIUM_SINGLE_THREADED)
                    part->mx.unlock();
#endif
                }
                delete h;
            }
        };
        struct listen_impl : i_lambda4<lambda0<void>*,
                transaction_impl*,
                const SODIUM_SHARED_PTR<impl::node>&,
                const SODIUM_SHARED_PTR<holder>&,
                bool> {
            listen_impl(const SODIUM_WEAK_PTR<node>& n_weak) : n_weak(n_weak) {}
            SODIUM_WEAK_PTR<node> n_weak;
            virtual lambda0<void>* operator () (transaction_impl* trans,
                        const SODIUM_SHARED_PTR<node>& target,
                        const SODIUM_SHARED_PTR<holder>& h,
                        bool suppressEarlierFirings) const {  // Register listener
                SODIUM_SHARED_PTR<node> n = n_weak.lock();
                if (n) {
                    SODIUM_FORWARD_LIST<light_ptr> firings;
                    holder* h = new holder(handler);   *** TO DO: BRING THIS UP-TO-DATE relative to C++11
                    {
#if !defined(SODIUM_SINGLE_THREADED)
                        trans->part->mx.lock();
#endif
                        if (n->link(h, target))
                            trans->to_regen = true;
#if !defined(SODIUM_SINGLE_THREADED)
                        trans->part->mx.unlock();
#endif
                        firings = n->firings;
                    }
                    if (!suppressEarlierFirings && firings.begin() != firings.end())
                        for (SODIUM_FORWARD_LIST<light_ptr>::iterator it = firings.begin(); it != firings.end(); it++)
                            h->handle(target, trans, *it);
                    return new lambda0<void>(new unregister(trans->part, n_weak, h));
                }
                else {
                    delete handler;
                    return NULL;
                }
            }
        };
#endif

        /*!
         * Creates an event, that values can be pushed into using impl::send(). 
         */
        SODIUM_TUPLE<event_, SODIUM_SHARED_PTR<node> > unsafe_new_event(
            event_::sample_now_func* p_sample_now_)
        {
            SODIUM_SHARED_PTR<event_::sample_now_func> p_sample_now(p_sample_now_);
            SODIUM_SHARED_PTR<node> n(new node);
            SODIUM_WEAK_PTR<node> n_weak(n);
            boost::intrusive_ptr<listen_impl_func<H_STRONG> > impl(
#if defined(SODIUM_NO_CXX11)
                new listen_impl_func<H_STRONG>(new listen_impl_func<H_STRONG>::closure(new listen_impl(n_weak)))
            );
#else
                new listen_impl_func<H_STRONG>(new listen_impl_func<H_STRONG>::closure([n_weak, p_sample_now] (transaction_impl* trans,
                        const SODIUM_SHARED_PTR<node>& target,
                        const SODIUM_SHARED_PTR<holder>& h,
                        bool suppressEarlierFirings) -> std::function<void()>* {  // Register listener
                    SODIUM_SHARED_PTR<node> n = n_weak.lock();
                    if (n) {
                        trans->part->mx.lock();
                        if (n->link(h.get(), target))
                            trans->to_regen = true;
                        trans->part->mx.unlock();
                        if (!suppressEarlierFirings && n->firings.begin() != n->firings.end()) {
                            SODIUM_FORWARD_LIST<light_ptr> firings = n->firings;
                            trans->prioritized(target, [target, h, firings] (transaction_impl* trans) {
                                for (SODIUM_FORWARD_LIST<light_ptr>::const_iterator it = firings.begin(); it != firings.end(); it++)
                                    h->handle(target, trans, *it);
                            });
                        }
                        partition* part = trans->part;
                        SODIUM_SHARED_PTR<holder>* h_keepalive = new SODIUM_SHARED_PTR<holder>(h);
                        return new std::function<void()>([part, n_weak, h_keepalive] () {  // Unregister listener
                            impl::transaction_ trans(part);
                            trans.impl()->last([n_weak, h_keepalive] () {
                                std::shared_ptr<node> n = n_weak.lock();
                                if (n)
                                    n->unlink((*h_keepalive).get());
                                delete h_keepalive;
                            });
                        });
                    }
                    else
                        return NULL;
                }))
            );
#endif
            n->listen_impl = boost::intrusive_ptr<listen_impl_func<H_NODE> >(
                reinterpret_cast<listen_impl_func<H_NODE>*>(impl.get()));
            boost::intrusive_ptr<listen_impl_func<H_EVENT> > li_event(
                reinterpret_cast<listen_impl_func<H_EVENT>*>(impl.get()));
            return SODIUM_MAKE_TUPLE(event_(li_event, p_sample_now), n);
        }

        event_sink_impl::event_sink_impl()
        {
        }

        event_ event_sink_impl::construct()
        {
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
            this->target = SODIUM_TUPLE_GET<1>(p);
            return SODIUM_TUPLE_GET<0>(p);
        }

        void event_sink_impl::send(transaction_impl* trans, const light_ptr& value) const
        {
            sodium::impl::send(target, trans, value);
        }

#if defined(SODIUM_NO_CXX11)
        struct hold_update_task : i_lambda0<void> {
            hold_update_task(const SODIUM_SHARED_PTR<behavior_state>& state) : state(state) {}
            SODIUM_SHARED_PTR<behavior_state> state;
            virtual void operator () () const {
                state->finalize();
            }
        };
        struct hold_handler : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            hold_handler(const SODIUM_SHARED_PTR<behavior_state>& state) : state(state) {}
            SODIUM_SHARED_PTR<behavior_state> state;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, transaction_impl* trans, const light_ptr& ptr) const {
                bool first = !state->update;
                state->update = boost::optional<light_ptr>(ptr);
                if (first)
                    trans->last(new hold_update_task(state));
                send(target, trans, ptr);
            }
        };
        struct hold_update_task_lazy : i_lambda0<void> {
            hold_update_task(const SODIUM_SHARED_PTR<behavior_state_lazy>& state) : state(state) {}
            SODIUM_SHARED_PTR<behavior_state_lazy> state;
            virtual void operator () () const {
                state->finalize();
            }
        };
        struct hold_handler_lazy : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            hold_handler(const SODIUM_SHARED_PTR<behavior_state>& state) : state(state) {}
            SODIUM_SHARED_PTR<behavior_state> state;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, transaction_impl* trans, const light_ptr& ptr) const {
                bool first = !state->update;
                state->update = boost::optional<light_ptr>(ptr);
                if (first)
                    trans->last(new hold_update_task_lazy(state));
                send(target, trans, ptr);
            }
        };
#endif

        SODIUM_SHARED_PTR<behavior_impl> hold(transaction_impl* trans0, const light_ptr& initValue, const event_& input)
        {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            if (input.is_never())
                return SODIUM_SHARED_PTR<behavior_impl>(new behavior_impl_constant(initValue));
            else {
#endif
                behavior_state state(initValue);
                SODIUM_SHARED_PTR<behavior_impl_concrete<behavior_state> > impl(
                    new behavior_impl_concrete<behavior_state>(input, state, std::shared_ptr<behavior_impl>())
                );
                impl->kill =
                    input.listen_raw(trans0, SODIUM_SHARED_PTR<node>(new node(SODIUM_IMPL_RANK_T_MAX)),
#if defined(SODIUM_NO_CXX11)
                    new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                        new hold_handler(state)
                    )
#else
                    new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                        [impl] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& ptr) {
                            bool first = !impl->state.update;
                            impl->state.update = boost::optional<light_ptr>(ptr);
                            if (first)
                                trans->last([impl] () { impl->state.finalize(); });
                            send(target, trans, ptr);
                        })
#endif
                    , false, true);
                return static_pointer_cast<behavior_impl, behavior_impl_concrete<behavior_state>>(impl);
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            }
#endif
        }

        SODIUM_SHARED_PTR<behavior_impl> hold_lazy(transaction_impl* trans0, const std::function<light_ptr()>& initValue, const event_& input)
        {
            behavior_state_lazy state(initValue);
            SODIUM_SHARED_PTR<behavior_impl_concrete<behavior_state_lazy> > impl(
                new behavior_impl_concrete<behavior_state_lazy>(input, state, std::shared_ptr<behavior_impl>())
            );
            impl->kill =
                input.listen_raw(trans0, SODIUM_SHARED_PTR<node>(new node(SODIUM_IMPL_RANK_T_MAX)),
#if defined(SODIUM_NO_CXX11)
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new hold_handler_lazy(state)
                )
#else
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [impl] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& ptr) {
                        bool first = !impl->state.update;
                        impl->state.update = boost::optional<light_ptr>(ptr);
                        if (first)
                            trans->last([impl] () { impl->state.finalize(); });
                        send(target, trans, ptr);
                    })
#endif
                , false, true);
            return static_pointer_cast<behavior_impl, behavior_impl_concrete<behavior_state_lazy>>(impl);
        }

        behavior_::behavior_()
        {
        }

        behavior_::behavior_(behavior_impl* impl)
            : impl(impl)
        {
        }

        behavior_::behavior_(const SODIUM_SHARED_PTR<behavior_impl>& impl)
            : impl(impl)
        {
        }

        behavior_::behavior_(const light_ptr& a)
            : impl(new behavior_impl_constant(a))
        {
        }

#if defined(SODIUM_NO_CXX11)
        struct value_sample_now : i_lambda1<void, vector<light_ptr>&> {
            value_sample_now(const lambda0<light_ptr>& sample)
            : sample(sample) {}
            lambda0<light_ptr> sample;
            virtual void operator () (vector<light_ptr>& items) const {
                items.push_back(sample());
            }
        };
#endif

        event_ behavior_::value_(transaction_impl* trans) const
        {
#if defined(SODIUM_NO_CXX11)
            return event_(
                updates_().p_listen_impl,
                new event_::sample_now_func(new value_sample_now(impl->sample))
#else
            auto impl = this->impl;
            return event_(
                updates_().p_listen_impl,
                new event_::sample_now_func([impl] (vector<light_ptr>& items) { items.push_back(impl->sample()); })
#endif
            ).last_firing_only_(trans);
        }

#if defined(SODIUM_CONSTANT_OPTIMIZATION)
        /*!
         * For optimization, if this behavior is a constant, then return its value.
         */
        boost::optional<light_ptr> behavior_::get_constant_value() const
        {
            return impl->updates.is_never() ? boost::optional<light_ptr>(impl->sample())
                                            : boost::optional<light_ptr>();
        }
#endif

        struct applicative_state {
            applicative_state() : fired(true) {}
            bool fired;
            boost::optional<light_ptr> f;
            boost::optional<light_ptr> a;
        };

#if defined(SODIUM_NO_CXX11)
        struct apply_const_a : i_lambda1<light_ptr, const light_ptr&> {
            apply_const_a(const light_ptr& a) : a(a) {}
            light_ptr a;
            virtual light_ptr operator () (const light_ptr& pf) const {
                const lambda1<light_ptr, const light_ptr&>& f =
                    *pf.cast_ptr<lambda1<light_ptr, const light_ptr&> >(NULL);
                return f(a);
            }
        };
#endif

        behavior_ apply(transaction_impl* trans0, const behavior_& bf, const behavior_& ba)
        {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            boost::optional<light_ptr> ocf = bf.get_constant_value();
            if (ocf) { // function is constant
#if defined(SODIUM_NO_CXX11)
                lambda1<light_ptr, const light_ptr&> f = *ocf.get().cast_ptr<lambda1<light_ptr, const light_ptr&> >(NULL);
#else
                auto f = *ocf.get().cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
#endif
                return impl::map_(trans0, f, ba);  // map optimizes to a constant where ba is constant
            }
            else {
                boost::optional<light_ptr> oca = ba.get_constant_value();
                if (oca) {  // 'a' value is constant but function is not
                    const light_ptr& a = oca.get();
#if defined(SODIUM_NO_CXX11)
                    return impl::map_(trans0, new apply_const_a(a), bf);
#else
                    return impl::map_(trans0, [a] (const light_ptr& pf) -> light_ptr {
                        const std::function<light_ptr(const light_ptr&)>& f =
                            *pf.cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
                        return f(a);
                    }, bf);
#endif
                }
                else {
#endif
                    // Non-constant case
                    SODIUM_SHARED_PTR<applicative_state> state(new applicative_state);

                    SODIUM_SHARED_PTR<impl::node> in_target(new impl::node);
                    SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event();
                    const SODIUM_SHARED_PTR<impl::node>& out_target = SODIUM_TUPLE_GET<1>(p);
                    if (in_target->link(NULL, out_target))
                        trans0->to_regen = true;
#if defined(SODIUM_NO_CXX11)
   *** // TO DO
#else
                    auto output = [state, out_target] (transaction_impl* trans) {
                        auto f = *state->f.get().cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
                        send(out_target, trans, f(state->a.get()));
                        state->fired = false;
                    };
                    auto kill1 = bf.value_(trans0).listen_raw(trans0, in_target,
                            new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                                [state, out_target, output] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& f) {
                                    state->f = f;
                                    if (state->fired) return;
                                    state->fired = true;
                                    trans->prioritized(out_target, output);
                                }
                            ), false, true);
                    auto kill2 = ba.value_(trans0).listen_raw(trans0, in_target,
                            new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                                [state, out_target, output] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& a) {
                                    state->a = a;
                                    if (state->fired) return;
                                    state->fired = true;
                                    trans->prioritized(out_target, output);
                                }
                            ), false, true);
#endif
                    // Suppress firing during first transaction.
                    trans0->last([state] () { state->fired = false; });
                    return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill1, kill2).hold_lazy_(
                        trans0, [bf, ba] () -> light_ptr {
                            auto f = *bf.impl->sample().cast_ptr<std::function<light_ptr(const light_ptr&)>>(NULL);
                            return f(ba.impl->sample());
                        }
                    );
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
                }
            }
#endif
        }

#if defined(SODIUM_NO_CXX11)
        struct send_wrapper : i_lambda3<void, const SODIUM_SHARED_PTR<node>&, transaction_impl*, const light_ptr&> {
            virtual void operator () (const SODIUM_SHARED_PTR<node>& n, transaction_impl* trans, const light_ptr& ptr) const {
                send(n, trans, ptr);
            }
        };
#endif

#if defined(SODIUM_NO_CXX11)
        event_ event_::add_cleanup_(transaction_impl* trans, lambda0<void>* cleanup) const
#else
        event_ event_::add_cleanup_(transaction_impl* trans, std::function<void()>* cleanup) const
#endif
        {
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event(
                p_sample_now != NULL
                ? new event_::sample_now_func(*p_sample_now)
                : NULL);
#if defined(SODIUM_NO_CXX11)
            lambda0<void>* kill = listen_raw(trans, SODIUM_TUPLE_GET<1>(p),
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new send_wrapper
                ),
#else
            auto kill = listen_raw(trans, std::get<1>(p),
                    new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(send),
#endif
                    false, false);
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill, cleanup);
        }

#if defined(SODIUM_NO_CXX11)
        struct map_sample_now : i_lambda1<void, vector<light_ptr>&> {
            map_sample_now(const SODIUM_SHARED_PTR<event_::sample_now_func>& p_sample_now,
                           const lambda1<light_ptr, const light_ptr&>& f)
            : p_sample_now(p_sample_now), f(f) {}
            SODIUM_SHARED_PTR<event_::sample_now_func> p_sample_now;
            lambda1<light_ptr, const light_ptr&> f;
            virtual void operator () (vector<light_ptr>& items) const {
                size_t start = items.size();
                (*p_sample_now)(items);
                for (vector<light_ptr>::iterator it = items.begin() + start; it != items.end(); ++it)
                    *it = f(*it);
            }
        };
        struct map_handler : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            map_handler(const lambda1<light_ptr, const light_ptr&>& f)
            : f(f) {}
            lambda1<light_ptr, const light_ptr&> f;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& ptr) const {
                send(target, trans, f(ptr));
            }
        };
#endif

        /*!
         * Map a function over this event to modify the output value.
         */
        event_ map_(transaction_impl* trans,
#if defined(SODIUM_NO_CXX11)
            const lambda1<light_ptr, const light_ptr&>& f,
#else
            const std::function<light_ptr(const light_ptr&)>& f,
#endif
            const event_& ev)
        {
#if defined(SODIUM_NO_CXX11)
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = impl::unsafe_new_event(
                ev.p_sample_now != NULL
                ? new event_::sample_now_func(new map_sample_now(ev.p_sample_now, f))
                : NULL);
            lambda0<void>* kill = ev.listen_raw(trans, SODIUM_TUPLE_GET<1>(p),
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new map_handler(f)), false);
#else
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
                        }), false, false);
#endif
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(kill);
        }

        behavior_ map_(transaction_impl* trans,
#if defined(SODIUM_NO_CXX11)
            const lambda1<light_ptr, const light_ptr&>& f,
#else
            const std::function<light_ptr(const light_ptr&)>& f,
#endif
            const behavior_& beh) {
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            boost::optional<light_ptr> ca = beh.get_constant_value();
            if (ca)
                return behavior_(f(ca.get()));
            else {
#endif
                auto impl = beh.impl;
                return map_(trans, f, beh.updates_()).hold_lazy_(trans, [f, impl] () -> light_ptr {
                    return f(impl->sample());
                });
#if defined(SODIUM_CONSTANT_OPTIMIZATION)
            }
#endif
        }

#if defined(SODIUM_NO_CXX11)
        struct switch_e_task : public i_lambda0<void> {
            switch_e_task(const SODIUM_SHARED_PTR<lambda0<void>*>& pKillInner,
                          const event_& ea,
                          impl::transaction_impl* trans1,
                          const SODIUM_SHARED_PTR<impl::node>& target)
            : pKillInner(pKillInner), ea(ea), trans1(trans1), target(target) {}
            SODIUM_SHARED_PTR<lambda0<void>*> pKillInner;
            event_ ea;
            impl::transaction_impl* trans1;
            SODIUM_SHARED_PTR<impl::node> target;
            virtual void operator () () const {
                KILL_ONCE(pKillInner);
                *pKillInner = ea.listen_raw(trans1, target, NULL, true);
            }
        };
        struct switch_e_handler : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            switch_e_handler(const SODIUM_SHARED_PTR<lambda0<void>*>& pKillInner) : pKillInner(pKillInner) {}
            SODIUM_SHARED_PTR<lambda0<void>*> pKillInner;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans1, const light_ptr& pea) const {
                const event_& ea = *pea.cast_ptr<event_>(NULL);
                trans1->last(new switch_e_task(pKillInner, ea, trans1, target));
            }
        };
        struct switch_e_kill : i_lambda0<void> {
            switch_e_kill(const SODIUM_SHARED_PTR<lambda0<void>*>& pKillInner) : pKillInner(pKillInner) {}
            SODIUM_SHARED_PTR<lambda0<void>*> pKillInner;
            virtual void operator () () const {
                KILL_ONCE(pKillInner);
            }
        };
#endif

        event_ switch_e(transaction_impl* trans0, const behavior_& bea)
        {
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = unsafe_new_event();
            const SODIUM_SHARED_PTR<impl::node>& target = SODIUM_TUPLE_GET<1>(p);
#if defined(SODIUM_NO_CXX11)
            SODIUM_SHARED_PTR<lambda0<void>*> pKillInner(new lambda0<void>*(
#else
            std::shared_ptr<function<void()>*> pKillInner(new function<void()>*(
#endif
                bea.impl->sample().cast_ptr<event_>(NULL)->listen_raw(trans0, target, NULL, false, true)
            ));

#if defined(SODIUM_NO_CXX11)
            lambda0<void>* killOuter = bea.updates_().listen_raw(trans0, target,
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new switch_e_handler(pKillInner)
                ),
#else
            auto killOuter = bea.updates_().listen_raw(trans0, target,
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [pKillInner] (const std::shared_ptr<impl::node>& target, impl::transaction_impl* trans1, const light_ptr& pea) {
                        const event_& ea = *pea.cast_ptr<event_>(NULL);
                        trans1->last([pKillInner, ea, trans1, target] () {
                            KILL_ONCE(pKillInner);
                            *pKillInner = ea.listen_raw(trans1, target, NULL, true, true);
                        });
                    }),
#endif
                false, true
            );
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(
#if defined(SODIUM_NO_CXX11)
                new lambda0<void>(new switch_e_kill(pKillInner))
#else
                new std::function<void()>([pKillInner] {
                    KILL_ONCE(pKillInner);
                })
#endif
                , killOuter);
        }

#if defined(SODIUM_NO_CXX11)
        struct switch_b_handler : i_lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&> {
            switch_b_handler(const SODIUM_SHARED_PTR<lambda0<void>*>& pKillInner) : pKillInner(pKillInner) {}
            SODIUM_SHARED_PTR<lambda0<void>*> pKillInner;
            virtual void operator () (const SODIUM_SHARED_PTR<impl::node>& target, impl::transaction_impl* trans, const light_ptr& pa) const {
                // Note: If any switch takes place during a transaction, then the
                // value().listen will always cause a sample to be fetched from the
                // one we just switched to. The caller will be fetching our output
                // using value().listen, and value() throws away all firings except
                // for the last one. Therefore, anything from the old input behaviour
                // that might have happened during this transaction will be suppressed.
                KILL_ONCE(pKillInner);
                const behavior_& ba = *pa.cast_ptr<behavior_>(NULL);
                *pKillInner = ba.value_(trans).listen_raw(trans, target, NULL, false);
            }
        };
#endif

        behavior_ switch_b(transaction_impl* trans0, const behavior_& bba)
        {
            auto za = [bba] () -> light_ptr { return bba.impl->sample().cast_ptr<behavior_>(NULL)->impl->sample(); };
#if defined(SODIUM_NO_CXX11)
            SODIUM_SHARED_PTR<lambda0<void>*> pKillInner(new lambda0<void>*(NULL));
#else
            SODIUM_SHARED_PTR<function<void()>*> pKillInner(new function<void()>*(NULL));
#endif
            SODIUM_TUPLE<impl::event_,SODIUM_SHARED_PTR<impl::node> > p = unsafe_new_event();
            auto out_target = SODIUM_TUPLE_GET<1>(p);
#if defined(SODIUM_NO_CXX11)
            lambda0<void>* killOuter =
#else
            auto killOuter =
#endif
                bba.value_(trans0).listen_raw(trans0, out_target,
#if defined(SODIUM_NO_CXX11)
                new lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>(
                    new switch_b_handler(pKillInner)
                )
#else
                new std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>(
                    [pKillInner] (const std::shared_ptr<impl::node>& target, transaction_impl* trans, const light_ptr& pa) {
                        // Note: If any switch takes place during a transaction, then the
                        // value().listen will always cause a sample to be fetched from the
                        // one we just switched to. The caller will be fetching our output
                        // using value().listen, and value() throws away all firings except
                        // for the last one. Therefore, anything from the old input behaviour
                        // that might have happened during this transaction will be suppressed.
                        KILL_ONCE(pKillInner);
                        const behavior_& ba = *pa.cast_ptr<behavior_>(NULL);
                        *pKillInner = ba.value_(trans).listen_raw(trans, target, NULL, false, true);
                    })
#endif
                , false, true);
            return SODIUM_TUPLE_GET<0>(p).unsafe_add_cleanup(
#if defined(SODIUM_NO_CXX11)
                new lambda0<void>(new switch_e_kill(pKillInner))
#else
                new std::function<void()>([pKillInner] {
                    KILL_ONCE(pKillInner);
                })
#endif
                , killOuter).hold_lazy_(trans0, za);
        }

    };  // end namespace impl
};  // end namespace sodium
