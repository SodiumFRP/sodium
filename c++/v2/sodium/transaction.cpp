/**
 * Copyright (c) 2012-2015, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#include <sodium/transaction.h>

namespace SODIUM_NAMESPACE {
    namespace impl {

        /*!
         * Return true if any changes were made. 
         */
        bool link_to(magic_ref<node_t>& node,
                     const magic_ref<std::function<void(transaction& trans, void*)>>& action,
                     const magic_ref<node_t>& target) {
            bool changed = ensure_bigger_than(node->rank, target);
            std::vector<node_t::target_t> listeners = node->listeners;
            listeners.push_back(node_t::target_t(action, target));
            node.assign(node_t(node->rank, listeners));
            return changed;
        }

        void unlink_to(magic_ref<node_t>& node,
                       const magic_ref<node_t>& target) {
            std::vector<node_t::target_t> listeners;
            for (auto it = node->listeners.begin(); it != node->listeners.end(); ++it)
                if (it->node.get_ptr() != target.get_ptr())
                    listeners.push_back(*it);
            node.assign(node_t(node->rank, listeners));
        }

        static bool ensure_bigger_than(magic_ref<node_t>& node, rank_t limit, std::set<node_t*>& visited) {
            if (node->rank > limit || visited.find(node.get_ptr()) != visited.end())
                return false;

            visited.insert(node.get_ptr());
            node.unsafe_get().rank = limit + 1;
            for (auto it = node->listeners.begin(); it != node->listeners.end(); ++it)
                ensure_bigger_than(it->node, node->rank, visited);
            return true;
        }

        bool ensure_bigger_than(magic_ref<node_t>& node, rank_t limit) {
            std::set<node_t*> visited;
            return ensure_bigger_than(node, limit, visited);
        }

    }  // end namespace impl
}  // end namespace SODIUM_NAMESPACE


