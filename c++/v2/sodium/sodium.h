/**
 * Copyright (c) 2012-2015, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM2_SODIUM_H_
#define _SODIUM2_SODIUM_H_

#include <sodium/impl/common.h>
#include <sodium/impl/magic_ref.h>
#include <sodium/transaction.h>
#include <forward_list>
#include <stdexcept>
#include <boost/variant.hpp>

namespace SODIUM_NAMESPACE {
    namespace impl {
        template <class A>
        struct stream_impl {
            stream_impl() : node(node_t(0)) {}
            stream_impl(
                const std::forward_list<impl::magic_ref<std::function<void()>>>& finalizers,
                const impl::magic_ref<node_t>& node,
                const std::forward_list<magic_ref<A>>& firings)
            : finalizers(finalizers),
              node(node),
              firings(firings)
            {
            }
            std::forward_list<impl::magic_ref<std::function<void()>>> finalizers;
            impl::magic_ref<node_t> node;
            std::forward_list<magic_ref<A>> firings;
        };

        template <class S>
        struct collect_state {
            collect_state(const std::function<S()>& s_lazy) : s_lazy(s_lazy) {}
            std::function<S()> s_lazy;
        };
    }
    template <class A> class cell;
    template <class A> class cell_sink;

    template <class A>
    class stream {
    template <class AA> friend class stream;
    template <class AA> friend class stream_loop;
    template <class AA> friend class cell;
    template <class AA> friend class cell_sink;
    template <class AA>
    friend stream<AA> filter_optional(const stream<boost::optional<AA>>& s);
    protected:
        impl::magic_ref<impl::stream_impl<A>> impl;

    private:
        stream(const impl::magic_ref<impl::stream_impl<A>>& impl) : impl(impl) {}
    public:
        stream() : impl(impl::stream_impl<A>()) {}
        virtual ~stream() {}
        std::function<void()> listen(const std::function<void(const A&)>& action) const {
            return listen_(impl::node_t::null, [action] (const transaction&, const void* v) {
                action(*(const A*)v);
            });
        }

        /*!
         * Transform the event's value according to the supplied function.
         */
        template <class B>
        stream<B> map(const std::function<B(const A&)>& f) const;

        /*!
         * Transform the event's value according to the supplied function.
         * same as map_. This is here to help avoid problems with namespace
         * conflicts with std::map.
         */
        template <class B>
        stream<B> map_(const std::function<B(const A&)>& f) const {
            return map(f);
        }

        /*!
         * Merge two streams of events of the same type.
         *
         * In the case where two event occurrences are simultaneous (i.e. both
         * within the same transaction), both will be delivered in the same
         * transaction. If the event firings are ordered for some reason, then
         * their ordering is retained. In many common cases the ordering will
         * be undefined.
         */
        stream<A> merge(const stream<A>& s) const;

        /*!
         * Merge two streams of events of the same type, combining simultaneous
         * event occurrences.
         *
         * In the case where multiple event occurrences are simultaneous (i.e. all
         * within the same transaction), they are combined using the same logic as
         * 'coalesce'.
         */
        stream<A> merge(const stream<A>& s, const std::function<A(const A&, const A&)>& f) const {
            return merge(s).coalesce(f);
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
        stream<A> coalesce(const std::function<A(const A&, const A&)>& f) const {
            transaction trans;
            return coalesce(trans, f);
        }
    protected:
        stream<A> coalesce(const transaction& trans, const std::function<A(const A&, const A&)>& f) const;
    public:

        /*!
         * Only keep event occurrences for which the predicate returns true.
         */
        stream<A> filter(const std::function<bool(const A&)>& f) const;

        stream<A> once() const {
            using namespace boost;
            optional<std::function<void()>> no_func;
            impl::magic_ref<optional<std::function<void()>>> r_kill(no_func);
            stream_with_send<A> out;
            r_kill.assign(make_optional(listen_(out.impl->node,
                [out, r_kill] (const transaction& trans, const void* va) {
                    if (*r_kill) {
                        const A& a = *(const A*)va;
                        out.send(trans, a);
                        (*r_kill).get()();
                        optional<std::function<void()>> no_func;
                        r_kill.assign(no_func);
                    }
                })));
            out.unsafe_add_cleanup(*r_kill.get());
            return out;
        }

        stream<A> gate(const cell<bool>& b) const;

        /*!
         * Variant of snapshot that throws away the event's value and captures the behavior's.
         */
        template <class B>
        stream<B> snapshot(const cell<B>& b) const;

        /*!
         * Sample the behavior at the time of the event firing. Note that the 'current value'
         * of the behavior that's sampled is the value as at the start of the transaction
         * before any state changes of the current transaction are applied through 'hold's.
         */
        template <class B, class C>
        stream<C> snapshot(const cell<B>& b, const std::function<C(const A&, const B&)>& f) const;

        cell<A> hold(const A& init_a) const;

        cell<A> hold_lazy(const std::function<A()>& lazy_init_a) const;

        /*!
         * Transform a behavior with a generalized state loop (a mealy machine). The function
         * is passed the input and the old state and returns the new state and output value.
         */
        template <class B>
        stream<B> accum_e_lazy(
            const std::function<B()>& initB0,
            const std::function<B(const A&, const B&)>& f
        ) const;

        /*!
         * Transform a behavior with a generalized state loop (a mealy machine). The function
         * is passed the input and the old state and returns the new state and output value.
         */
        template <class B>
        cell<B> accum_lazy(
            const std::function<B()>& initB0,
            const std::function<B(const A&, const B&)>& f
        ) const {
            return accum_e_lazy<B>(initB0, f).hold_lazy(initB0);
        }

        /*!
         * Transform a behavior with a generalized state loop (a mealy machine). The function
         * is passed the input and the old state and returns the new state and output value.
         */
        template <class B>
        stream<B> accum_e(
            const B& initB0,
            const std::function<B(const A&, const B&)>& f
        ) const {
            return accum_e_lazy([initB0] () { return initB0; }, f);
        }

        /*!
         * Transform a behavior with a generalized state loop (a mealy machine). The function
         * is passed the input and the old state and returns the new state and output value.
         */
        template <class B>
        cell<B> accum(
            const B& initB0,
            const std::function<B(const A&, const B&)>& f
        ) const {
            return accum_lazy<B>([initB0] () { return initB0; }, f);
        }

        /*!
         * Transform a behavior with a generalized state loop (a mealy machine). The function
         * is passed the input and the old state and returns the new state and output value.
         */
        template <class S, class B>
        stream<B> collect_lazy(
            const std::function<S()>& initS0,
            const std::function<std::tuple<B, S>(const A&, const S&)>& f
        ) const;

        /*!
         * Transform a behavior with a generalized state loop (a mealy machine). The function
         * is passed the input and the old state and returns the new state and output value.
         */
        template <class S, class B>
        stream<B> collect(
            const S& initS,
            const std::function<std::tuple<B, S>(const A&, const S&)>& f
        ) const
        {
            return collect_lazy<S, B>([initS] () -> S { return initS; }, f);
        }

        stream<A> add_cleanup(const std::function<void()>& cleanup0) const {
            return stream<A>(impl::magic_ref<impl::stream_impl<A>>(
                    add_cleanup_impl(cleanup0)
                ));
        }

    protected:
        void unsafe_add_cleanup(const std::function<void()>& cleanup0) const {
            impl::stream_impl<A> a = add_cleanup_impl(cleanup0);
            impl.assign(a);
        }

        impl::stream_impl<A> add_cleanup_impl(const std::function<void()>& cleanup0) const {
            impl::magic_ref<std::function<void()>> cleanup(cleanup0);
            std::forward_list<impl::magic_ref<std::function<void()>>> finalizers(impl->finalizers);
            finalizers.push_front(cleanup);
            return impl::stream_impl<A>(
                    finalizers,
                    impl->node,
                    impl->firings
                );
        }

        std::function<void()> listen_(const impl::magic_ref<impl::node_t>& target,
                const std::function<void(const transaction&, const void*)>& action) const {
            transaction trans;
            return listen(target, trans, action, false);
        }
        std::function<void()> listen(const impl::magic_ref<impl::node_t>& target,
                const transaction& trans,
                const std::function<void(const transaction&, const void*)>& action0,
                bool suppress_earlier_firings) const {
            impl::magic_ref<std::function<void(const transaction& trans, const void*)>> action(action0);
            transaction::listeners_lock.lock();
            impl::target_ref_t target_ref;
            if (link_to(impl->node, action, target, target_ref))
                trans.impl->to_regen = true;
            transaction::listeners_lock.unlock();
            // reverse the firings list when we copy it, because we've stored it in reverse order.
            std::forward_list<impl::magic_ref<A>> firings;
            for (auto it = impl->firings.begin(); it != impl->firings.end(); ++it)
                firings.push_front(*it);
            if (!suppress_earlier_firings && firings.begin() != firings.end()) { 
                trans.prioritized(target, [firings, action] (const transaction& trans) {
                    for (auto it = firings.begin(); it != firings.end(); ++it) {
                        transaction::in_callback++;
                        try {
                            action.get()(trans, (const void*)&it->get());
                            transaction::in_callback--;
                        }
                        catch (...) {
                            transaction::in_callback--;
                            throw;
                        }
                    }
                });
            }
            impl::magic_ref<impl::node_t> node = impl->node;
            return [node, target_ref] () {
                transaction::listeners_lock.lock();
                impl::unlink_to(node, target_ref);
                transaction::listeners_lock.unlock();
            };
        }

        /**
         * Clean up the output by discarding any firing other than the last one. 
         */
        stream<A> last_firing_only(const transaction& trans) {
            return coalesce(trans, [] (const A&, const A& second) { return second; });
        }
    };

    template <class A>
    class stream_with_send : public stream<A> {
    template <class AA> friend class stream;
    template <class AA> friend class cell;
    template <class AA>
    friend stream<AA> filter_optional(const stream<boost::optional<AA>>& s);
    public:
        stream_with_send() {}
        virtual ~stream_with_send() {}

        static void send(impl::magic_ref<impl::stream_impl<A>> impl,
                         const transaction& trans, const A& a) {
            if (impl->firings.begin() == impl->firings.end()) {
                trans.last([impl] () {
                    impl.unsafe_get().firings.clear();
                });
            }
            impl.unsafe_get().firings.push_front(impl::magic_ref<A>(a));

            transaction::listeners_lock.lock();
            std::vector<impl::node_t::target_t> listeners(impl->node->listeners);
            transaction::listeners_lock.unlock();
            for (auto it = listeners.begin(); it != listeners.end(); ++it) {
                auto action = it->action;
                trans.prioritized(it->node, [action, a] (const transaction& trans) {
                    transaction::in_callback++;
                    try {
                        action.get()(trans, (const void*)&a);
                        transaction::in_callback--;
                    }
                    catch (...) {
                        transaction::in_callback--;
                        throw;
                    }
                });
            }
        }

    protected:
        void send(const transaction& trans, const A& a) const {
            send(this->impl, trans, a);
        }
    };

    template <class A>
    struct stream_sink : stream_with_send<A> {
        stream_sink() {}
        virtual ~stream_sink() {}

        void send(const A& a) const {
            transaction trans;
            if (transaction::in_callback > 0)
                throw std::runtime_error("You are not allowed to use send() inside a Sodium callback");
            stream_with_send<A>::send(trans, a);
        }
	};

    template <class A>
    template <class B>
    stream<B> stream<A>::map(const std::function<B(const A&)>& f) const {
        stream_with_send<B> out;
        out.unsafe_add_cleanup(listen_(out.impl->node, [out, f] (const transaction& trans, const void* va) {
            const A& a = *(const A*)va;
            out.send(trans, f(a));
        }));
        return out;
    }

    template <class A>
    stream<A> stream<A>::merge(const stream<A>& s) const {
        stream_with_send<A> out;
        impl::magic_ref<impl::node_t> left(impl::node_t(0));
        const impl::magic_ref<impl::node_t>& right(out.impl->node);
        impl::magic_ref<std::function<void(const transaction& trans, const void*)>> null_action;
        impl::target_ref_t target_ref;
        impl::link_to(left, null_action, right, target_ref);
        std::function<void(const transaction& trans, const void* va)> h =
            [out] (const transaction& trans, const void* va) {
                out.send(trans, *(const A*)va);
            };
        auto kill1 = listen_(left, h);
        auto kill2 = s.listen_(right, h);
        return out.add_cleanup([kill1, kill2, left, target_ref] () {
            kill1();
            kill2();
            unlink_to(left, target_ref);
        });
    }

    template <class A>
    stream<A> stream<A>::coalesce(const transaction& trans, const std::function<A(const A&, const A&)>& f) const {
        using namespace std;
        using namespace boost;
        stream_with_send<A> out;
        shared_ptr<optional<A>> p_state(new optional<A>);
        auto kill = listen(out.impl->node, trans, [p_state, f, out] (const transaction& trans, const void* va) {
            const A& a = *(const A*)va;
            if (*p_state)
                *p_state = optional<A>(f(p_state->get(), a));
            else {
                trans.prioritized(out.impl->node, [out, p_state] (const transaction& trans) {
                    out.send(trans, p_state->get());
                    *p_state = optional<A>();
                });
                *p_state = optional<A>(a);
            }
        }, false);
        return out.add_cleanup(kill);
    }

    template <class A>
    stream<A> stream<A>::filter(const std::function<bool(const A&)>& f) const {
        stream_with_send<A> out;
        auto kill = listen_(out.impl->node, [f, out] (const transaction& trans, const void *va) {
            const A& a = *(const A*)va;
            if (f(a)) out.send(trans, a);
        });
        return out.add_cleanup(kill);
    }

    template <class A>
    stream<A> filter_optional(const stream<boost::optional<A>>& s) {
        stream_with_send<A> out;
        out.unsafe_add_cleanup(s.listen_(out.impl->node, [out] (const transaction& trans, const void *va) {
            const boost::optional<A>& oa = *(const boost::optional<A>*)va;
            if (oa) out.send(trans, oa.get());
        }));
        return out;
    }

    template <class A>
    stream<A> stream<A>::gate(const cell<bool>& b) const {
        using namespace boost;
        return filter_optional(snapshot<bool,optional<A>>(b, [] (const A& a, const bool& b) {
            return b ? optional<A>(a)
                     : optional<A>();
        }));
    }

    template <class A>
    template <class B>
    stream<B> stream<A>::snapshot(const cell<B>& b) const {
        return snapshot<B,B>(b, [] (const A&, const B& b) { return b; });
    }

    template <class A>
    template <class B, class C>
    stream<C> stream<A>::snapshot(const cell<B>& b, const std::function<C(const A&, const B&)>& f) const {
        stream_with_send<C> out;
        out.unsafe_add_cleanup(listen_(out.impl->node, [out, b, f] (const transaction& trans, const void* va) {
            const A& a = *(const A*)va;
            out.send(trans, f(a, b.sample_no_trans()));
        }));
        return out;
    }

    template <class A>
    template <class B>
    stream<B> stream<A>::accum_e_lazy(
        const std::function<B()>& initB0,
        const std::function<B(const A&, const B&)>& f
    ) const
    {
        transaction trans;
        impl::collect_state<B> initB(initB0);
        impl::magic_ref<impl::collect_state<B> > state(initB);
        stream_with_send<B> out;
        out.unsafe_add_cleanup(listen(out.impl->node, trans,
            [state, f, out] (const transaction& trans, const void* va) {
                B outB = f(*(const A*)va, state->s_lazy());
                state.assign(impl::collect_state<B>([outB] () { return outB; }));
                out.send(trans, outB);
            }, false));
        return out;
    }

    template <class A>
    template <class S, class B>
    stream<B> stream<A>::collect_lazy(
        const std::function<S()>& initS0,
        const std::function<std::tuple<B, S>(const A&, const S&)>& f
    ) const
    {
        transaction trans;
        impl::collect_state<S> initS(initS0);
        impl::magic_ref<impl::collect_state<S> > state(initS);
        stream_with_send<B> out;
        out.unsafe_add_cleanup(listen(out.impl->node, trans,
            [state, f, out] (const transaction& trans, const void* va) {
                std::tuple<B,S> outsSt = f(*(const A*)va, state->s_lazy());
                const S& new_s = std::get<1>(outsSt);
                state.assign(impl::collect_state<S>([new_s] () { return new_s; }));
                out.send(trans, std::get<0>(outsSt));
            }, false));
        return out;
    }

    template <class A>
    struct stream_loop : stream<A> {
        impl::magic_ref<stream<A>> out;
        stream_loop() {
            if (!transaction::get_current_transaction())
                throw std::runtime_error("stream_loop/cell_loop must be used within an explicit transaction");
        }

        void loop(const stream<A>& out) const {
            if (this->out)
                throw std::runtime_error("stream_loop looped more than once");
            this->out.assign(out);
            auto impl(this->impl);
            stream<A>::unsafe_add_cleanup(out.listen_(impl->node,
                [impl] (const transaction& trans, const void* va) {
                    const A& a = *(const A*)va;
                    stream_with_send<A>::send(impl, trans, a);
                }));
        }
    };

    namespace impl {
        template <class A>
        struct cell_impl {
            cell_impl(const A& a) : value(a), cleanup([] () {}) {}
            cell_impl(const stream<A>& str, const impl::magic_ref<A>& value,
                      const impl::magic_ref<std::function<A()>>& lazy_init_value,
                      const impl::magic_ref<std::function<void()>>& cleanup)
                : str(str), value(value), really_clean_up(false), cleanup(cleanup),
                  lazy_init_value(lazy_init_value) {}
            ~cell_impl() {
                if (really_clean_up)
                    (*cleanup)();
            }
            stream<A> str;
            impl::magic_ref<A> value;
            impl::magic_ref<A> value_update;
            bool really_clean_up;
            impl::magic_ref<std::function<void()>> cleanup;
            impl::magic_ref<std::function<A()>> lazy_init_value;

            A sample() const {
                return value ? *value
                             : (*lazy_init_value)();
            }
        };
    }

    template <class A>
    class cell {
    template <class AA> friend class stream;
    protected:
        impl::magic_ref<impl::cell_impl<A>> impl;
        cell(const stream<A>& str, const impl::magic_ref<A>& init_a,
                                   const impl::magic_ref<std::function<A()>>& lazy_init_value) {
            transaction trans;
            auto impl(this->impl);
            auto kill = str.listen(impl::node_t::null, trans, [impl] (const transaction& trans, const void* va) {
                const A& a = *(const A*)va;
                if (!impl->value_update) {
                    trans.last([impl] () {
                        impl.unsafe_get().value = impl->value_update;
                        impl.unsafe_get().lazy_init_value.reset();
                        impl.unsafe_get().value_update.reset();
                    });
                }
                impl->value_update.assign(a);
            }, false);
            impl.assign(impl::cell_impl<A>(str, init_a, lazy_init_value, kill));
            impl.unsafe_get().really_clean_up = true;
        }
    public:
        cell(const A& a) : impl(impl::cell_impl<A>(a)) {}
    
        /*!
         * Sample the behavior's current value.
         *
         * This should generally be avoided in favour of listen(..) so you don't
         * miss any updates, but in many circumstances it makes sense.
         *
         * It can be best to use it inside an explicit transaction.
         * For example, a b.sample() inside an explicit transaction along with a
         * b.updates().listen(..) will capture the current value and any updates without risk
         * of missing any in between.
         */
        A sample() const {
            transaction trans;
            return sample_no_trans();
        }

        /*!
         * Returns an event giving the updates to a behavior. If this behavior was created
         * by a hold, then this gives you back an event equivalent to the one that was held.
         */
        stream<A> updates() const {
            return impl->str;
        }

        /*!
         * Returns an event describing the value of a behavior, where there's an initial event
         * giving the current value.
         */
        stream<A> value() const {
            transaction trans;
            return stream<A>(value(trans));
        }

    protected:
        stream<A> value(const transaction& trans1) const {
            stream_sink<unit> sSpark;
            trans1.prioritized(sSpark.impl->node, [sSpark] (const transaction& trans2) {
                sSpark.send(unit());
            });
            stream<A> sInitial = sSpark.snapshot(*this);
            return sInitial.merge(updates()).last_firing_only(trans1);
        }

    public:
        std::function<A()> sample_lazy() const {
            using namespace boost;
            transaction trans;
            variant<impl::magic_ref<impl::cell_impl<A>>, impl::magic_ref<A>> v_impl(impl);
            impl::magic_ref<variant<impl::magic_ref<impl::cell_impl<A>>, impl::magic_ref<A>>> lazy(
                    v_impl
                );
            trans.last([lazy] () {
                auto impl = boost::get<impl::magic_ref<impl::cell_impl<A>>>(*lazy);
                lazy.assign(variant<impl::magic_ref<impl::cell_impl<A>>, impl::magic_ref<A>>(
                    impl->value_update ? *impl->value_update : impl->sample()
                ));
            });
            return [lazy] () -> A {
                transaction trans;
                if (const impl::magic_ref<A>* refa = boost::get<impl::magic_ref<A>>(&*lazy))
                    return **refa;
                else {
                    auto impl = boost::get<impl::magic_ref<impl::cell_impl<A>>>(*lazy); 
                    return impl->sample();
                }
            };
        }

        /*!
         * Transform a behavior with a generalized state loop (a mealy machine). The function
         * is passed the input and the old state and returns the new state and output value.
         */
        template <class S, class B>
        cell<B> collect_lazy(
            const std::function<S()>& initS0,
            const std::function<std::tuple<B, S>(const A&, const S&)>& f
        ) const
        {
            transaction trans;
            auto ea = updates() /*.coalesce([] (const A&, const A& snd) -> A { return snd; })*/;
            impl::magic_ref<std::function<A()>> za_lazy(sample_lazy());
            impl::magic_ref<std::function<S()>> initS(initS0);
            impl::magic_ref<std::function<std::tuple<B,S>()>> zbs([za_lazy, initS, f] () -> std::tuple<B,S> {
                return f((*za_lazy)(), (*initS)());
            });
            impl::magic_ref<impl::collect_state<S> > state(impl::collect_state<S>([zbs] () -> S {
                return std::get<1>((*zbs)());
            }));
            stream_with_send<B> out;
            out.unsafe_add_cleanup(ea.listen(out.impl->node, trans,
                [state, f, out] (const transaction& trans, const void* va) {
                    std::tuple<B,S> outsSt = f(*(const A*)va, state->s_lazy());
                    const S& new_s = std::get<1>(outsSt);
                    state.assign(impl::collect_state<S>([new_s] () { return new_s; }));
                    out.send(trans, std::get<0>(outsSt));
                }, false));
            return out.hold_lazy([zbs] () -> B {
                return std::get<0>((*zbs)());
            });
        }

        /*!
         * Transform a behavior with a generalized state loop (a mealy machine). The function
         * is passed the input and the old state and returns the new state and output value.
         */
        template <class S, class B>
        cell<B> collect(
            const S& initS,
            const std::function<std::tuple<B, S>(const A&, const S&)>& f
        ) const
        {
            return collect_lazy<S, B>([initS] () -> S { return initS; }, f);
        }

    protected:
        A sample_no_trans() const {
            return impl->sample();
        }
    };

    template <class A>
    cell<A> stream<A>::hold(const A& init_a) const {
        return cell<A>(*this, impl::magic_ref<A>(init_a), impl::magic_ref<std::function<A()>>());
    }

    template <class A>
    cell<A> stream<A>::hold_lazy(const std::function<A()>& lazy_init_a) const {
        return cell<A>(*this, impl::magic_ref<A>(), impl::magic_ref<std::function<A()>>(lazy_init_a));
    }

    template <class A>
    class cell_sink : public cell<A> {
    public:
        cell_sink(const A& initValue) : cell<A>(stream<A>(), impl::magic_ref<A>(initValue), impl::magic_ref<std::function<A()>>()) {}
        void send(const A& a) {
            transaction trans;
            stream_with_send<A>::send(this->impl->str.impl, trans, a); 
        }
    };
#if 0
	/**
	 * Create a behavior with the specified initial value, that gets updated
     * by the values coming through the event. The 'current value' of the behavior
     * is notionally the value as it was 'at the start of the transaction'.
     * That is, state updates caused by event firings get processed at the end of
     * the transaction.
     */
	public final Cell<A> hold(final A init_a) {
		return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
			public Cell<A> apply(Transaction trans) {
			    return new Cell<A>(lastFiringOnly(trans), init_a);
			}
		});
	}

	final Cell<A> holdLazy(final Lambda0<A> init_a) {
		return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
			public Cell<A> apply(Transaction trans) {
			    return new LazyCell<A>(lastFiringOnly(trans), init_a);
			}
		});
	}

    /**
     * Merge two streams of events of the same type.
     *
     * In the case where two event occurrences are simultaneous (i.e. both
     * within the same transaction), both will be delivered in the same
     * transaction. If the event firings are ordered for some reason, then
     * their ordering is retained. In many common cases the ordering will
     * be undefined.
     */
	public Stream<A> merge(final Stream<A> eb)
	{
	    return Stream.<A>merge(this, eb);
	}

	/**
	 * Push this event occurrence onto a new transaction. Same as split() but works
     * on a single value.
	 */
	public final Stream<A> defer()
	{
	    final StreamSink<A> out = new StreamSink<A>();
	    Listener l1 = listen_(out.node, new TransactionHandler<A>() {
	        public void run(Transaction trans, final A a) {
	            trans.post(new Runnable() {
                    public void run() {
                        Transaction trans = new Transaction();
                        try {
                            out.send(trans, a);
                        } finally {
                            trans.close();
                        }
                    }
	            });
	        }
	    });
	    return out.addCleanup(l1);
	}

	/**
	 * Push each event occurrence in the list onto a new transaction.
	 */
    public static final <A, C extends Collection<A>> Stream<A> split(Stream<C> s)
    {
	    final StreamSink<A> out = new StreamSink<A>();
	    Listener l1 = s.listen_(out.node, new TransactionHandler<C>() {
	        public void run(Transaction trans, final C as) {
	            trans.post(new Runnable() {
                    public void run() {
                        for (A a : as) {
                            Transaction trans = new Transaction();
                            try {
                                out.send(trans, a);
                            } finally {
                                trans.close();
                            }
                        }
                    }
	            });
	        }
	    });
	    return out.addCleanup(l1);
    }

    /**
     * Clean up the output by discarding any firing other than the last one. 
     */
    final Stream<A> lastFiringOnly(Transaction trans)
    {
        return coalesce(trans, new Lambda2<A,A,A>() {
        	public A apply(A first, A second) { return second; }
        });
    }

    public Stream<A> merge(Stream<A> eb, Lambda2<A,A,A> f)
    {
        return merge(eb).coalesce(f);
    }

    /**
     * Filter the empty values out, and strip the Optional wrapper from the present ones.
     */
    public static final <A> Stream<A> filterOptional(final Stream<Optional<A>> ev)
    {
        final StreamSink<A> out = new StreamSink<A>();
        Listener l = ev.listen_(out.node, new TransactionHandler<Optional<A>>() {
        	public void run(Transaction trans2, Optional<A> oa) {
	            if (oa.isPresent()) out.send(trans2, oa.get());
	        }
        });
        return out.addCleanup(l);
    }

    /**
     * Let event occurrences through only when the behavior's value is True.
     * Note that the behavior's value is as it was at the start of the transaction,
     * that is, no state changes from the current transaction are taken into account.
     */
    public final Stream<A> gate(Cell<Boolean> bPred)
    {
        return snapshot(bPred, new Lambda2<A,Boolean,A>() {
        	public A apply(A a, Boolean pred) { return pred ? a : null; }
        }).filterNotNull();
    }

    /**
     * Transform an event with a generalized state loop (a mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     */
    public final <B,S> Stream<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        return Transaction.<Stream<B>>run(() -> {
            final Stream<A> ea = this;
            StreamLoop<S> es = new StreamLoop<S>();
            Cell<S> s = es.hold(initState);
            Stream<Tuple2<B,S>> ebs = ea.snapshot(s, f);
            Stream<B> eb = ebs.map(new Lambda1<Tuple2<B,S>,B>() {
                public B apply(Tuple2<B,S> bs) { return bs.a; }
            });
            Stream<S> es_out = ebs.map(new Lambda1<Tuple2<B,S>,S>() {
                public S apply(Tuple2<B,S> bs) { return bs.b; }
            });
            es.loop(es_out);
            return eb;
        });
    }

    /**
     * Accumulate on input event, outputting the new state each time.
     */
    public final <S> Cell<S> accum(final S initState, final Lambda2<A, S, S> f)
    {
        return Transaction.<Cell<S>>run(() -> {
            final Stream<A> ea = this;
            StreamLoop<S> es = new StreamLoop<S>();
            Cell<S> s = es.hold(initState);
            Stream<S> es_out = ea.snapshot(s, f);
            es.loop(es_out);
            return es_out.hold(initState);
        });
    }

    /**
     * Throw away all event occurrences except for the first one.
     */
    public final Stream<A> once()
    {
        // This is a bit long-winded but it's efficient because it deregisters
        // the listener.
        final Stream<A> ev = this;
        final Listener[] la = new Listener[1];
        final StreamSink<A> out = new StreamSink<A>();
        la[0] = ev.listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans, A a) {
	            if (la[0] != null) {
                    out.send(trans, a);
	                la[0].unlisten();
	                la[0] = null;
	            }
	        }
        });
        return out.addCleanup(la[0]);
    }

    Stream<A> addCleanup(Listener cleanup)
    {
        finalizers.add(cleanup);
        return this;
    }

	@Override
	protected void finalize() throws Throwable {
		for (Listener l : finalizers)
			l.unlisten();
	}
#endif

#if defined(SODIUM_OLD_NAME_COMPATIBILITY)
    template <class A>
    struct event : stream<A> {
        event() {}
        event(const stream<A>& other) : stream<A>(other) {}
        virtual ~event() {}
    };
    template <class A>
    struct event_sink : stream_sink<A> {
        event_sink() {}
        event_sink(const stream_sink<A>& other) : stream_sink<A>(other) {}
        virtual ~event_sink() {}
    };
    template <class A>
    struct event_loop : stream_loop<A> {
        event_loop() {}
        event_loop(const stream_loop<A>& other) : event_loop<A>(other) {}
        virtual ~event_loop() {}
    };
    template <class A>
    struct behavior : cell<A> {
        behavior(const A& initValue) : cell<A>(initValue) {}
        behavior(const cell<A>& other) : cell<A>(other) {}
    };
    template <class A>
    struct behavior_sink : cell_sink<A> {
        behavior_sink(const A& initValue) : cell_sink<A>(initValue) {}
    };
#endif

#if 0
package sodium;

public class Cell<A> {
	protected final Stream<A> event;
	A value;
	A valueUpdate;
	private Listener cleanup;
    protected Lambda0<A> lazyInitValue;  // Used by LazyCell

	/**
	 * A behavior with a constant value.
	 */
    public Cell(A value)
    {
    	this.event = new Stream<A>();
    	this.value = value;
    }

    Cell(final Stream<A> event, A init_a)
    {
    	this.event = event;
    	this.value = init_a;
    	Transaction.run(new Handler<Transaction>() {
    		public void run(Transaction trans1) {
	    		Cell.this.cleanup = event.listen(Node.NULL, trans1, new TransactionHandler<A>() {
	    			public void run(Transaction trans2, A a) {
			    		if (Cell.this.valueUpdate == null) {
			    			trans2.last(new Runnable() {
			    				public void run() {
				    				Cell.this.value = Cell.this.valueUpdate;
				    				Cell.this.lazyInitValue = null;
				    				Cell.this.valueUpdate = null;
				    			}
			    			});
			    		}
			    		Cell.this.valueUpdate = a;
			    	}
	    		}, false);
    		}
    	});
    }

    /**
     * @return The value including any updates that have happened in this transaction.
     */
    final A newValue()
    {
    	return valueUpdate == null ? sampleNoTrans() :  valueUpdate;
    }

    /**
     * An event that gives the updates for the behavior. If this behavior was created
     * with a hold, then updates() gives you an event equivalent to the one that was held.
     */
    public final Stream<A> updates()
    {
    	return event;
    }

    /**
     * An event that is guaranteed to fire once when you listen to it, giving
     * the current value of the behavior, and thereafter behaves like updates(),
     * firing for each update to the behavior's value.
     */
    public final Stream<A> value()
    {
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(Transaction trans) {
        		return value(trans);
        	}
        });
    }

    final Stream<A> value(Transaction trans1)
    {
    	StreamSink<Unit> sSpark = new StreamSink<Unit>();
        trans1.prioritized(sSpark.node, new Handler<Transaction>() {
            public void run(Transaction trans2) {
                sSpark.send(trans2, Unit.UNIT);
            }
        });
    	Stream<A> sInitial = sSpark.<A>snapshot(this);
        return sInitial.merge(updates()).lastFiringOnly(trans1);
    }

    /**
     * Transform the behavior's value according to the supplied function.
     */
	public final <B> Cell<B> map(Lambda1<A,B> f)
	{
		return updates().map(f).holdLazy(() -> f.apply(sampleNoTrans()));
	}

	/**
	 * Lift a binary function into behaviors.
	 */
	public final <B,C> Cell<C> lift(final Lambda2<A,B,C> f, Cell<B> b)
	{
		Lambda1<A, Lambda1<B,C>> ffa = new Lambda1<A, Lambda1<B,C>>() {
			public Lambda1<B,C> apply(final A aa) {
				return new Lambda1<B,C>() {
					public C apply(B bb) {
						return f.apply(aa,bb);
					}
				};
			}
		};
		Cell<Lambda1<B,C>> bf = map(ffa);
		return apply(bf, b);
	}

	/**
	 * Lift a binary function into behaviors.
	 */
	public static final <A,B,C> Cell<C> lift(Lambda2<A,B,C> f, Cell<A> a, Cell<B> b)
	{
		return a.lift(f, b);
	}

	/**
	 * Lift a ternary function into behaviors.
	 */
	public final <B,C,D> Cell<D> lift(final Lambda3<A,B,C,D> f, Cell<B> b, Cell<C> c)
	{
		Lambda1<A, Lambda1<B, Lambda1<C,D>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C,D>>>() {
			public Lambda1<B, Lambda1<C,D>> apply(final A aa) {
				return new Lambda1<B, Lambda1<C,D>>() {
					public Lambda1<C,D> apply(final B bb) {
						return new Lambda1<C,D>() {
							public D apply(C cc) {
								return f.apply(aa,bb,cc);
							}
						};
					}
				};
			}
		};
		Cell<Lambda1<B, Lambda1<C, D>>> bf = map(ffa);
		return apply(apply(bf, b), c);
	}

	/**
	 * Lift a ternary function into behaviors.
	 */
	public static final <A,B,C,D> Cell<D> lift(Lambda3<A,B,C,D> f, Cell<A> a, Cell<B> b, Cell<C> c)
	{
		return a.lift(f, b, c);
	}

	/**
	 * Lift a quaternary function into behaviors.
	 */
	public final <B,C,D,E> Cell<E> lift(final Lambda4<A,B,C,D,E> f, Cell<B> b, Cell<C> c, Cell<D> d)
	{
		Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,E>>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,E>>>>() {
			public Lambda1<B, Lambda1<C, Lambda1<D,E>>> apply(final A aa) {
				return new Lambda1<B, Lambda1<C, Lambda1<D,E>>>() {
					public Lambda1<C, Lambda1<D, E>> apply(final B bb) {
						return new Lambda1<C, Lambda1<D,E>>() {
							public Lambda1<D,E> apply(final C cc) {
                                return new Lambda1<D, E>() {
                                    public E apply(D dd) {
                                        return f.apply(aa,bb,cc,dd);
                                    }
                                };
							}
						};
					}
				};
			}
		};
		Cell<Lambda1<B, Lambda1<C, Lambda1<D, E>>>> bf = map(ffa);
		return apply(apply(apply(bf, b), c), d);
	}

	/**
	 * Lift a quaternary function into behaviors.
	 */
	public static final <A,B,C,D,E> Cell<E> lift(Lambda4<A,B,C,D,E> f, Cell<A> a, Cell<B> b, Cell<C> c, Cell<D> d)
	{
		return a.lift(f, b, c, d);
	}

	/**
	 * Apply a value inside a behavior to a function inside a behavior. This is the
	 * primitive for all function lifting.
	 */
	public static <A,B> Cell<B> apply(final Cell<Lambda1<A,B>> bf, final Cell<A> ba)
	{
    	return Transaction.apply(new Lambda1<Transaction, Cell<B>>() {
    		public Cell<B> apply(Transaction trans0) {
                final StreamSink<B> out = new StreamSink<B>();

                class ApplyHandler implements Handler<Transaction> {
                    ApplyHandler(Transaction trans0) {
                        resetFired(trans0);  // We suppress firing during the first transaction
                    }
                    boolean fired = true;
                    Lambda1<A,B> f = null;
                    A a = null;
                    @Override
                    public void run(Transaction trans1) {
                        if (fired) 
                            return;

                        fired = true;
                        trans1.prioritized(out.node, new Handler<Transaction>() {
                            public void run(Transaction trans2) {
                                out.send(trans2, f.apply(a));
                            }
                        });
                        resetFired(trans1);
                    }
                    void resetFired(Transaction trans1) {
                        trans1.last(() -> { fired = false; });
                    }
                }

                Node out_target = out.node;
                Node in_target = new Node(0);
                in_target.linkTo(null, out_target);
                final ApplyHandler h = new ApplyHandler(trans0);
                Listener l1 = bf.value().listen_(in_target, new TransactionHandler<Lambda1<A,B>>() {
                    public void run(Transaction trans1, Lambda1<A,B> f) {
                        h.f = f;
                        h.run(trans1);
                    }
                });
                Listener l2 = ba.value().listen_(in_target, new TransactionHandler<A>() {
                    public void run(Transaction trans1, A a) {
                        h.a = a;
                        h.run(trans1);
                    }
                });
                return out.addCleanup(l1).addCleanup(l2).addCleanup(
                    new Listener() {
                        public void unlisten() {
                            in_target.unlinkTo(out_target);
                        }
                    }).holdLazy(() -> bf.sampleNoTrans().apply(ba.sampleNoTrans()));
            }
        });
	}

	/**
	 * Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
	 */
	public static <A> Cell<A> switchC(final Cell<Cell<A>> bba)
	{
	    return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
	        public Cell<A> apply(Transaction trans0) {
                Lambda0<A> za = () -> bba.sampleNoTrans().sampleNoTrans();
                final StreamSink<A> out = new StreamSink<A>();
                TransactionHandler<Cell<A>> h = new TransactionHandler<Cell<A>>() {
                    private Listener currentListener;
                    @Override
                    public void run(Transaction trans2, Cell<A> ba) {
                        // Note: If any switch takes place during a transaction, then the
                        // value().listen will always cause a sample to be fetched from the
                        // one we just switched to. The caller will be fetching our output
                        // using value().listen, and value() throws away all firings except
                        // for the last one. Therefore, anything from the old input behaviour
                        // that might have happened during this transaction will be suppressed.
                        if (currentListener != null)
                            currentListener.unlisten();
                        currentListener = ba.value(trans2).listen(out.node, trans2, new TransactionHandler<A>() {
                            public void run(Transaction trans3, A a) {
                                out.send(trans3, a);
                            }
                        }, false);
                    }
        
                    @Override
                    protected void finalize() throws Throwable {
                        if (currentListener != null)
                            currentListener.unlisten();
                    }
                };
                Listener l1 = bba.value().listen_(out.node, h);
                return out.addCleanup(l1).holdLazy(za);
            }
        });
	}
	
	/**
	 * Unwrap an event inside a behavior to give a time-varying event implementation.
	 */
	public static <A> Stream<A> switchS(final Cell<Stream<A>> bea)
	{
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(final Transaction trans) {
                return switchS(trans, bea);
        	}
        });
    }

	private static <A> Stream<A> switchS(final Transaction trans1, final Cell<Stream<A>> bea)
	{
        final StreamSink<A> out = new StreamSink<A>();
        final TransactionHandler<A> h2 = new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            out.send(trans2, a);
	        }
        };
        TransactionHandler<Stream<A>> h1 = new TransactionHandler<Stream<A>>() {
            private Listener currentListener = bea.sampleNoTrans().listen(out.node, trans1, h2, false);

            @Override
            public void run(final Transaction trans2, final Stream<A> ea) {
                trans2.last(new Runnable() {
                	public void run() {
	                    if (currentListener != null)
	                        currentListener.unlisten();
	                    currentListener = ea.listen(out.node, trans2, h2, true);
	                }
                });
            }

            @Override
            protected void finalize() throws Throwable {
                if (currentListener != null)
                    currentListener.unlisten();
            }
        };
        Listener l1 = bea.updates().listen(out.node, trans1, h1, false);
        return out.addCleanup(l1);
	}

    /**
     * Transform a behavior with a generalized state loop (a mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     */
    public final <B,S> Cell<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        return Transaction.<Cell<B>>run(() -> {
            final Stream<A> ea = updates().coalesce(new Lambda2<A,A,A>() {
                public A apply(A fst, A snd) { return snd; }
            });
            final Lambda0<Tuple2<B, S>> zbs = () -> f.apply(sampleNoTrans(), initState);
            StreamLoop<Tuple2<B,S>> ebs = new StreamLoop<Tuple2<B,S>>();
            Cell<Tuple2<B,S>> bbs = ebs.holdLazy(zbs);
            Cell<S> bs = bbs.map(new Lambda1<Tuple2<B,S>,S>() {
                public S apply(Tuple2<B,S> x) {
                    return x.b;
                }
            });
            Stream<Tuple2<B,S>> ebs_out = ea.snapshot(bs, f);
            ebs.loop(ebs_out);
            return bbs.map(new Lambda1<Tuple2<B,S>,B>() {
                public B apply(Tuple2<B,S> x) {
                    return x.a;
                }
            });
        });
    }

	@Override
	protected void finalize() throws Throwable {
	    if (cleanup != null)
            cleanup.unlisten();
	}

	/**
	 * Listen for firings of this event. The returned Listener has an unlisten()
	 * method to cause the listener to be removed. This is the observer pattern.
     */
	public final Listener listen(final Handler<A> action) {
        return Transaction.apply(new Lambda1<Transaction, Listener>() {
        	public Listener apply(final Transaction trans) {
                return value().listen(action);
			}
		});
	}
}
#endif

}  // end namespace SODIUM_NAMESPACE

#endif

