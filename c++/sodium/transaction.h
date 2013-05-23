/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM_TRANSACTION_H_
#define _SODIUM_TRANSACTION_H_

#include <boost/shared_ptr.hpp>
#include <boost/optional.hpp>
#include <sodium/unit.h>
#include <pthread.h>
#include <map>
#include <set>
#include <list>

namespace sodium {
    namespace impl {
        class event_impl;
    }
}

void intrusive_ptr_add_ref(sodium::impl::event_impl* p);
void intrusive_ptr_release(sodium::impl::event_impl* p);
#include <boost/intrusive_ptr.hpp>

namespace sodium {

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

    struct partition {
        partition();
        ~partition();
        mutex mx;
        int depth;
        pthread_key_t key;
        bool processing_post;
        std::list<std::function<void()>> postQ;
        void post(const std::function<void()>& action);
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
        class node;

        class node
        {
            public:
                struct target {
                    target(
                        void* holder,
                        const std::shared_ptr<node>& n
                    ) : holder(holder),
                        n(n) {}
                    void* holder;
                    std::shared_ptr<node> n;
                };

                struct listen_impl_func {
                    typedef std::function<std::function<void()>*(
                        transaction_impl*,
                        const std::shared_ptr<impl::node>&,
                        std::function<void(const std::shared_ptr<impl::node>&, transaction_impl*, const light_ptr&)>*,
                        bool)> closure;
                    listen_impl_func(const closure& func, std::function<void()>* cleanup1)
                        : func(func)
                    {
                        if (cleanup1 != NULL)
                            cleanups.push_back(cleanup1);
                    }
                    listen_impl_func(const closure& func)
                        : func(func) {}
                    ~listen_impl_func()
                    {
                        for (auto it = cleanups.begin(); it != cleanups.end(); ++it) {
                            (**it)();
                            delete *it;
                        }
                    }
                    closure func;
                    std::list<std::function<void()>*> cleanups;
                    std::list<std::shared_ptr<listen_impl_func>> children;
                };

            public:
                node() : rank(0) {}

                unsigned long long rank;
                std::list<node::target> targets;
                std::list<light_ptr> firings;
                std::shared_ptr<listen_impl_func> listen_impl;

                void link(void* holder, const std::shared_ptr<node>& target);
                bool unlink(void* holder);

            private:
                void ensure_bigger_than(std::set<node*>& visited, unsigned long long limit);
        };

        template <class A>
        struct ordered_value {
            ordered_value() : tid(-1) {}
            long long tid;
            boost::optional<A> oa;
        };

        struct entryID {
            entryID() : id(0) {}
            entryID(unsigned long long id) : id(id) {}
            unsigned long long id;
            entryID succ() const { return entryID(id+1); }
            inline bool operator < (const entryID& other) const { return id < other.id; }
        };

        unsigned long long rankOf(const std::shared_ptr<node>& target);

        struct prioritized_entry {
            prioritized_entry(const std::shared_ptr<node>& target,
                              const std::function<void(transaction_impl*)>& action)
                : target(target), action(action)
            {
            }
            std::shared_ptr<node> target;
            std::function<void(transaction_impl*)> action;
        };

        struct transaction_impl {
            transaction_impl(partition* part);
            ~transaction_impl();
            partition* part;
            entryID next_entry_id;
            std::map<entryID, prioritized_entry> entries;
            std::multimap<unsigned long long, entryID> prioritizedQ;
            std::list<std::function<void()>> lastQ;

            void prioritized(const std::shared_ptr<impl::node>& target,
                             const std::function<void(impl::transaction_impl*)>& action);
            void last(const std::function<void()>& action);

            bool to_regen;
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
        virtual void dispatch(impl::transaction_impl* impl,
            const std::function<void()>& transactional,
            const std::function<void()>& post) = 0;
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
        };
    };

    template <class P = def_part>
    class transaction : public impl::transaction_
    {
        private:
            transaction(const transaction<P>& other) {}
            transaction<P>& operator = (const transaction<P>& other) { return *this; };
        public:
            transaction() : transaction_(P::part()) {}
    };

    class simple_policy : public policy
    {
    public:
        simple_policy();
        virtual ~simple_policy();
        virtual impl::transaction_impl* current_transaction(partition* part);
        virtual void initiate(impl::transaction_impl* impl);
        virtual void dispatch(impl::transaction_impl* impl,
            const std::function<void()>& transactional,
            const std::function<void()>& post);
    };
};  // end namespace sodium

#endif

