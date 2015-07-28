/**
 * Copyright (c) 2012-2014, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM_TRANSACTION_H_
#define _SODIUM_TRANSACTION_H_

#include <sodium/config.h>
#include <sodium/count_set.h>
#include <sodium/light_ptr.h>
#include <sodium/lock_pool.h>
#include <boost/optional.hpp>
#include <boost/intrusive_ptr.hpp>
#include <sodium/unit.h>
#include <map>
#include <set>
#include <list>
#include <memory>
#ifdef __linux
#include <pthread.h>
#else
#include <pthread/pthread.h>
#endif
#if defined(SODIUM_NO_CXX11)
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/fusion/adapted/boost_tuple.hpp>
#include <boost/fusion/include/boost_tuple.hpp>
#else
#include <forward_list>
#include <tuple>
#endif

namespace sodium {

#if defined(SODIUM_NO_CXX11)
    template <class A>
    struct i_lambda0
    {
        i_lambda0() {}
        virtual ~i_lambda0() {}

        virtual A operator () () const = 0;
    };

    template <class A>
    struct lambda0 {
        lambda0() {}
        lambda0(i_lambda0<A>* f) : f(f) {}
        A operator () () const { return (*f)(); }
        SODIUM_SHARED_PTR<i_lambda0<A> > f;
    };

    template <class A, class B>
    struct i_lambda1
    {
        i_lambda1() {}
        virtual ~i_lambda1() {}

        virtual A operator () (B b) const = 0;
    };

    template <class A, class B>
    struct lambda1 {
        lambda1() {}
        lambda1(i_lambda1<A,B>* f) : f(f) {}
        A operator () (B b) const { return (*f)(b); }
        SODIUM_SHARED_PTR<i_lambda1<A,B> > f;
    };

    template <class A, class B, class C>
    struct i_lambda2
    {
        i_lambda2() {}
        virtual ~i_lambda2() {}

        virtual A operator () (B b, C c) const = 0;
    };

    template <class A, class B, class C>
    struct lambda2 {
        lambda2() {}
        lambda2(i_lambda2<A,B,C>* f) : f(f) {}
        A operator () (B b, C c) const { return (*f)(b, c); }
        SODIUM_SHARED_PTR<i_lambda2<A,B,C> > f;
    };

    template <class A, class B, class C, class D>
    struct i_lambda3
    {
        i_lambda3() {}
        virtual ~i_lambda3() {}

        virtual A operator () (B b, C c, D d) const = 0;
    };

    template <class A, class B, class C, class D>
    struct lambda3 {
        lambda3() {}
        lambda3(i_lambda3<A,B,C,D>* f) : f(f) {}
        A operator () (B b, C c, D d) const { return (*f)(b, c, d); }
        SODIUM_SHARED_PTR<i_lambda3<A,B,C,D> > f;
    };

    template <class A, class B, class C, class D, class E>
    struct i_lambda4
    {
        i_lambda4() {}
        virtual ~i_lambda4() {}

        virtual A operator () (B b, C c, D d, E e) const = 0;
    };

    template <class A, class B, class C, class D, class E>
    struct lambda4 {
        lambda4() {}
        lambda4(i_lambda4<A,B,C,D,E>* f) : f(f) {}
        A operator () (B b, C c, D d, E e) const { return (*f)(b, c, d, e); }
        SODIUM_SHARED_PTR<i_lambda4<A,B,C,D,E> > f;
    };
#endif

#if !defined(SODIUM_SINGLE_THREADED)
    class mutex
    {
    private:
        pthread_mutex_t mx;
        // ensure we don't copy or assign a mutex by value
        mutex(const mutex& other) {}
        mutex& operator = (const mutex& other) { return *this; }
    public:
        mutex();
        ~mutex();
        void lock()
        {
            pthread_mutex_lock(&mx);
        }
        void unlock()
        {
            pthread_mutex_unlock(&mx);
        }
    };
#endif

    struct partition {
        partition();
        ~partition();
#if !defined(SODIUM_SINGLE_THREADED)
        mutex mx;
#endif
        int depth;
#if !defined(SODIUM_SINGLE_THREADED)
        pthread_key_t key;
#endif
        bool processing_post;
#if defined(SODIUM_NO_CXX11)
        std::list<lambda0<void> > postQ;
        void post(const lambda0<void>& action);
#else
        std::list<std::function<void()>> postQ;
        void post(const std::function<void()>& action);
#endif
        void process_post();
    };

    /*!
     * The default partition which gets chosen when you don't specify one.
     */
    struct def_part {
        static partition* part();
    };

    namespace impl {
        struct transaction_impl;

        typedef unsigned long rank_t;
        #define SODIUM_IMPL_RANK_T_MAX ULONG_MAX
        
        class holder;

        class node;
        template <class Allocator>
        struct listen_impl_func {
#if defined(SODIUM_NO_CXX11)
            typedef lambda4<lambda0<void>*,
                transaction_impl*,
                const SODIUM_SHARED_PTR<impl::node>&,
                const SODIUM_SHARED_PTR<holder>&,
                bool> closure;
#else
            typedef std::function<std::function<void()>*(
                transaction_impl*,
                const std::shared_ptr<impl::node>&,
                const SODIUM_SHARED_PTR<holder>&,
                bool)> closure;
#endif
            listen_impl_func(closure* func)
                : func(func) {}
            ~listen_impl_func()
            {
                assert(cleanups.begin() == cleanups.end() && func == NULL);
            }
            count_set counts;
            closure* func;
#if defined(SODIUM_NO_CXX11)
            SODIUM_FORWARD_LIST<lambda0<void>*> cleanups;
#else
            SODIUM_FORWARD_LIST<std::function<void()>*> cleanups;
#endif
            inline void update_and_unlock(spin_lock* l) {
                if (func && !counts.active()) {
                    counts.inc_strong();
                    l->unlock();
#if defined(SODIUM_NO_CXX11)
                    for (std::list<lambda0<void>*>::iterator it = cleanups.begin(); it != cleanups.end(); ++it) {
#else
                    for (auto it = cleanups.begin(); it != cleanups.end(); ++it) {
#endif
                        (**it)();
                        delete *it;
                    }
                    cleanups.clear();
                    delete func;
                    func = NULL;
                    l->lock();
                    counts.dec_strong();
                }
                if (!counts.alive()) {
                    l->unlock();
                    delete this;
                }
                else
                    l->unlock();
            }
        };

        class holder {
            public:
                holder(
#if defined(SODIUM_NO_CXX11)
                    lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>* handler
#else
                    std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handler
#endif
                ) : handler(handler) {}
                ~holder() {
                    delete handler;
                }
                void handle(const SODIUM_SHARED_PTR<node>& target, transaction_impl* trans, const light_ptr& value) const;

            private:
#if defined(SODIUM_NO_CXX11)
                lambda3<void, const SODIUM_SHARED_PTR<impl::node>&, transaction_impl*, const light_ptr&>* handler;
#else
                std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>* handler;
#endif
        };

        struct H_EVENT {};
        struct H_STRONG {};
        struct H_NODE {};

        void intrusive_ptr_add_ref(sodium::impl::listen_impl_func<sodium::impl::H_EVENT>* p);
        void intrusive_ptr_release(sodium::impl::listen_impl_func<sodium::impl::H_EVENT>* p);
        void intrusive_ptr_add_ref(sodium::impl::listen_impl_func<sodium::impl::H_STRONG>* p);
        void intrusive_ptr_release(sodium::impl::listen_impl_func<sodium::impl::H_STRONG>* p);
        void intrusive_ptr_add_ref(sodium::impl::listen_impl_func<sodium::impl::H_NODE>* p);
        void intrusive_ptr_release(sodium::impl::listen_impl_func<sodium::impl::H_NODE>* p);

        inline bool alive(const boost::intrusive_ptr<listen_impl_func<H_STRONG> >& li) {
            return li && li->func != NULL;
        }

        inline bool alive(const boost::intrusive_ptr<listen_impl_func<H_EVENT> >& li) {
            return li && li->func != NULL;
        }

        class node
        {
            public:
                struct target {
                    target(
                        void* h,
                        const SODIUM_SHARED_PTR<node>& n
                    ) : h(h),
                        n(n) {}

                    void* h;
                    SODIUM_SHARED_PTR<node> n;
                };

            public:
                node();
                node(rank_t rank);
                ~node();

                rank_t rank;
                SODIUM_FORWARD_LIST<node::target> targets;
                SODIUM_FORWARD_LIST<light_ptr> firings;
                SODIUM_FORWARD_LIST<boost::intrusive_ptr<listen_impl_func<H_EVENT> > > sources;
                boost::intrusive_ptr<listen_impl_func<H_NODE> > listen_impl;

                bool link(void* holder, const SODIUM_SHARED_PTR<node>& target);
                void unlink(void* holder);

            private:
                bool ensure_bigger_than(std::set<node*>& visited, rank_t limit);
        };
    }
}

namespace sodium {
    namespace impl {

        template <class A>
        struct ordered_value {
            ordered_value() : tid(-1) {}
            long long tid;
            boost::optional<A> oa;
        };

        struct entryID {
            entryID() : id(0) {}
            entryID(rank_t id) : id(id) {}
            rank_t id;
            entryID succ() const { return entryID(id+1); }
            inline bool operator < (const entryID& other) const { return id < other.id; }
        };

        rank_t rankOf(const SODIUM_SHARED_PTR<node>& target);

        struct prioritized_entry {
#if defined(SODIUM_NO_CXX11)
            prioritized_entry(const SODIUM_SHARED_PTR<node>& target,
                              const lambda1<void, transaction_impl*>& action)
#else
            prioritized_entry(const SODIUM_SHARED_PTR<node>& target,
                              const std::function<void(transaction_impl*)>& action)
#endif
                : target(target), action(action)
            {
            }
            SODIUM_SHARED_PTR<node> target;
#if defined(SODIUM_NO_CXX11)
            lambda1<void, transaction_impl*> action;
#else
            std::function<void(transaction_impl*)> action;
#endif
        };

        struct transaction_impl {
            transaction_impl(partition* part);
            ~transaction_impl();
            partition* part;
            entryID next_entry_id;
            std::map<entryID, prioritized_entry> entries;
            std::multiset<std::pair<rank_t, entryID>> prioritizedQ;
#if defined(SODIUM_NO_CXX11)
            std::list<lambda0<void> > lastQ;
#else
            std::list<std::function<void()>> lastQ;
#endif
            bool to_regen;

            void prioritized(const SODIUM_SHARED_PTR<impl::node>& target,
#if defined(SODIUM_NO_CXX11)
                             const lambda1<void, impl::transaction_impl*>& action);
            void last(const lambda0<void>& action);
#else
                             const std::function<void(impl::transaction_impl*)>& action);
            void last(const std::function<void()>& action);
#endif

            void check_regen();
            void process_transactional();
        };
    };

    class policy {
    public:
        policy() {}
        virtual ~policy() {}

        static policy* get_global();
        static void set_global(policy* policy);

        /*!
         * Get the current thread's active transaction for this partition, or NULL
         * if none is active.
         */
        virtual impl::transaction_impl* current_transaction(partition* part) = 0;

        virtual void initiate(impl::transaction_impl* impl) = 0;

        /*!
         * Dispatch the processing for this transaction according to the policy.
         * Note that post() will delete impl, so don't reference it after that.
         */
#if defined(SODIUM_NO_CXX11)
        virtual void dispatch(impl::transaction_impl* impl,
            const lambda0<void>& transactional,
            const lambda0<void>& post) = 0;
#else
        virtual void dispatch(impl::transaction_impl* impl,
            const std::function<void()>& transactional,
            const std::function<void()>& post) = 0;
#endif
    };

    namespace impl {
        class transaction_ {
        private:
            transaction_impl* impl_;
            transaction_(const transaction_& other) {}
            transaction_& operator = (const transaction_& other) { return *this; };
        public:
            transaction_(partition* part);
            ~transaction_();
            impl::transaction_impl* impl() const { return impl_; }
        protected:
            void close();
        };
    };

    class transaction : public impl::transaction_
    {
        private:
            // Disallow copying
            transaction(const transaction& other) : impl::transaction_(def_part::part()) {}
            // Disallow copying
            transaction& operator = (const transaction& other) { return *this; };
        public:
            transaction() : impl::transaction_(def_part::part()) {}
            /*!
             * The destructor will close the transaction, so normally close() isn't needed.
             * But, in some cases you might want to close it earlier, and close() will do this for you.
             */
            inline void close() { impl::transaction_::close(); }
    };

    class simple_policy : public policy
    {
    public:
        simple_policy();
        virtual ~simple_policy();
        virtual impl::transaction_impl* current_transaction(partition* part);
        virtual void initiate(impl::transaction_impl* impl);
        virtual void dispatch(impl::transaction_impl* impl,
#if defined(SODIUM_NO_CXX11)
            const lambda0<void>& transactional,
            const lambda0<void>& post);
#else
            const std::function<void()>& transactional,
            const std::function<void()>& post);
#endif
    };
}  // end namespace sodium

#endif

