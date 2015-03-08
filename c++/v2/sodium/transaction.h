/**
 * Copyright (c) 2012-2015, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM2_TRANSACTION_H_
#define _SODIUM2_TRANSACTION_H_

#include <sodium/unit.h>
#include <sodium/impl/magic_ref.h>
#include <stdint.h>
#include <memory>

namespace SODIUM_NAMESPACE {
    class transaction;
    template <class A> class stream;
    template <class A> class stream_with_send;
    template <class A> class stream_sink;
    template <class A> class stream_loop;
    template <class A> class cell;

    namespace impl {
        typedef uint32_t rank_t;
        static const rank_t null_rank = 0x80000000;
        typedef uint64_t seq_t;
        typedef uint32_t target_ref_t;

        struct node_t {
            static node_t null;
            struct target_t {
                static target_ref_t next_target_ref;
                target_t(
                    const magic_ref<std::function<void(const transaction& trans, const void*)>>& action,
                    const magic_ref<node_t>& node,
                    target_ref_t target_ref
                ) : action(action), node(node), target_ref(target_ref) {}
                magic_ref<std::function<void(const transaction& trans, const void*)>> action;
                magic_ref<node_t> node;
                target_ref_t target_ref;
            };
            node_t(rank_t rank) : rank(rank) {}
            node_t(rank_t rank, const std::vector<target_t>& listeners) : rank(rank), listeners(listeners) {}
            rank_t rank;
            std::vector<target_t> listeners;
        };

        /*!
         * Return true if any changes were made. 
         */
        bool link_to(const magic_ref<node_t>& node,
                     const magic_ref<std::function<void(const transaction& trans, const void*)>>& action,
                     const magic_ref<node_t>& target,
                     target_ref_t& target_ref);
        void unlink_to(const magic_ref<node_t>& node,
                       target_ref_t target_ref);

        class transaction_impl {
        private:
            transaction_impl(const transaction_impl& other) {}
            transaction_impl operator = (const transaction_impl& other) { return *this; }
        public:
            bool active;
            static impl::mutex transaction_lock;
            template <class A> friend class sodium::stream;
            friend class transaction;
            struct entry {
                entry(const magic_ref<node_t>& node,
                      const std::shared_ptr<std::function<void(const transaction&)>>& action);
                entry(const magic_ref<node_t>& node,
                      const std::shared_ptr<std::function<void(const transaction&)>>& action,
                      seq_t seq);
                rank_t rank;
                magic_ref<node_t> node;
                std::shared_ptr<std::function<void(const transaction&)>> action;
                static seq_t next_seq;
                seq_t seq;
                bool operator == (const entry& other) const {
                    return rank == other.rank && seq == other.seq;
                };
                bool operator < (const entry& other) const {
                    return rank == other.rank ? seq < other.seq
                                              : rank < other.rank;
                }
            };

            bool to_regen;
            std::set<entry> prioritized_q;
            std::list<std::shared_ptr<std::function<void()>>> last_q;
            std::list<std::shared_ptr<std::function<void()>>> post_q;

            transaction_impl();
            ~transaction_impl();
            void close(const std::shared_ptr<transaction_impl>& impl);
            void check_regen();
        };
    }  // end namespace impl

    class transaction {
    friend class impl::transaction_impl;
    template <class A> friend class sodium::stream;
    template <class A> friend class sodium::stream_with_send;
    template <class A> friend class sodium::stream_sink;
    template <class A> friend class sodium::stream_loop;
    template <class A> friend class sodium::cell;
    private:
        std::shared_ptr<impl::transaction_impl> impl;
        static impl::mutex listeners_lock;
        static std::weak_ptr<impl::transaction_impl> current_transaction_1;
        static std::shared_ptr<impl::transaction_impl> current_transaction_2;
        static int in_callback;
        transaction(const std::shared_ptr<impl::transaction_impl>& impl);
    public:
        transaction();
        ~transaction() {}
        void close();
    private:
        static std::shared_ptr<impl::transaction_impl> get_current_transaction_impl();
        static boost::optional<transaction> get_current_transaction();
        void prioritized(const impl::magic_ref<impl::node_t>& node,
                         const std::function<void(const transaction&)>& action) const;
        void post(const std::function<void()>& action) const;
        void last(const std::function<void()>& action) const;
    };
}  // end namespace sodium

#endif

