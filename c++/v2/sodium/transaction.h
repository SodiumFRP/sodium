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

namespace SODIUM_NAMESPACE {
    class transaction;

    namespace impl {
        typedef uint32_t rank_t;
        static const rank_t null_rank = 0x80000000;
        typedef uint64_t seq_t;

        struct node_t {
            node(rank_t rank) : rank(rank) {}
            node(rank_t rank, const std::vector<target_t>& listeners) : rank(rank), listeners(listeners) {}
            struct target_t {
                target_t(
                    const magic_ref<std::function<void(transaction& trans, void*)>>& action,
                    const magic_ref<node>& n
                ) : action(action), n(n) {}
                magic_ref<std::function<void(transaction& trans, void*)>> action;
                magic_ref<node_t> node_t;
            };
            rank_t rank;
            std::vector<target_t> listeners;
        };

        /*!
         * Return true if any changes were made. 
         */
        bool link_to(magic_ref<node_t>& node,
                     const magic_ref<std::function<void(transaction& trans, void*)>>& action,
                     const magic_ref<node_t>& target);
        void unlink_to(magic_ref<node_t>& node,
                       const magic_ref<node_t>& target);
        bool ensure_bigger_than(magic_ref<node_t>& node, rank_t limit);

        class transaction_impl {
        private:
            struct entry {
                entry(const magic_ref<node_t>& node,
                      const std::shared_ptr<std::function<void(const magic_ref<transaction_impl>&)>>& action);
                entry(const magic_ref<node_t>& node,
                      const std::shared_ptr<std::function<void(const magic_ref<transaction_impl>&)>>& action,
                      seq_t seq);
                rank_t rank;
                magic_ref<node_t> node;
                std::shared_ptr<std::function<void(const magic_ref<transaction_impl>&)>> action;
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

            static mutex listeners_lock;
            bool to_regen;
            std::set<entry> prioritized_q;
            std::list<std::shared_ptr<std::function<void()>>> last_q;
            std::list<std::shared_ptr<std::function<void()>>> post_q;

            void check_regen();
        public:
            transaction_impl()
            : to_regen(false)
            {
            }
        };

        class transaction {
        private:
            std::shared_ptr<transaction_impl> impl;
            static mutex transaction_lock;
            static std::shared_ptr<transaction_impl> current_transaction;
            static int in_callback;
            transaction(const std::shared_ptr<transaction_impl>& impl);
        public:
            transaction();
            ~transaction() { close(); }
            void close();
            boost::optional<transaction> get_current_transaction();
            void prioritized(magic_ref<node_t>& node, const std::function<void(const magic_ref<transaction_impl>&)>& action);
            void post(const std::function<void()>& action);
            void last(const std::function<void()>& action);
        };
    };
}  // end namespace sodium

#endif

