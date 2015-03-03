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
    };
}  // end namespace sodium

#endif

