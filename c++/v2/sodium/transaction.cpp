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

        bool ensure_bigger_than(magic_ref<node_t>& node, rank_t limit) {
            std::set<node_t*> visited;
            return ensure_bigger_than(node, limit, visited);
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

        mutex transaction_impl::listeners_lock;
        seq_t transaction_impl::entry::next_seq = 0;

        transaction_impl::entry::entry(const magic_ref<node_t>& node,
              const std::shared_ptr<std::function<void(const magic_ref<transaction_impl>&)>>& action)
        : rank(node->rank), node(node), action(action), seq(++next_seq)
        {
        }

        transaction_impl::entry::entry(const magic_ref<node_t>& node,
              const std::shared_ptr<std::function<void(const magic_ref<transaction_impl>&)>>& action,
              seq_t seq)
        : rank(node->rank), node(node), action(action), seq(seq)
        {
        }

        transaction_impl::transaction_impl()
        : to_regen(false)
        {
        }

        void transaction_impl::check_regen() {
            if (to_regen) {
                to_regen = false;
                std::set<entry> neu;
                for (auto it = prioritized_q.begin(); it != prioritized_q.end(); ++it)
                    neu.insert(entry(it->node, it->action, it->seq));
                prioritized_q = neu;
            }
        }

    }  // end namespace impl

    mutex transaction::transaction_lock;
    std::weak_ptr<transaction_impl> transaction::current_transaction;
    int transaction::in_callback = 0;

    transaction::transaction() {
        transaction_lock.lock();
        impl = current_transaction.lock();
        if (!impl) {
            impl = new transaction_impl;
            current_transaction = impl;
        }
    }

    transaction::transaction(const std::shared_ptr<transaction_impl>& impl)
        : impl(impl)
    {
        if (this.impl)
            transaction_lock.lock();
    }

    void transaction::close() {
        if (impl) {
            while (true) {
                check_regen();
                if (prioritized_q.begin() == prioritized_q.end())
                    break;
                entry e = *prioritized_q.begin();
                prioritized_q.erase(prioritized_q.begin());
                (*e->action)(impl);
            }
            for (auto it = last_q.begin(); it != last_q.end(); ++it)
                (*it)();
            last_q.clear();
            for (auto it = post_q.begin(); it != post_q.end(); ++it)
                (*it)();
            post_q.clear();
            impl.reset();
            transaction_lock.unlock();
        }
    }

    transaction transaction::get_current_transaction() {
        using namespace std;
        using namespace boost;
        optional<transaction> o_trans;
        transaction_lock.lock();
        shared_ptr<impl::transaction_impl> s_trans = current_transaction.lock();
        if (s_trans)
            o_trans = make_optional(transaction(s_trans));
        transaction_lock.unlock();
        return o_trans;
    }

    void transaction::prioritized(magic_ref<node_t>& node, const std::function<void(const magic_ref<transaction_impl>&)>& action)
    {
        impl->prioritizedQ.insert(transaction_impl::entry(node,
            std::shared_ptr<std::function<void(const magic_ref<transaction_impl>&)>>(
                    new std::function<void(const magic_ref<transaction_impl>&)(action)
                )));
    }

    void transaction::post(const std::function<void()>& action) {
        impl->post_q(new std::shared_ptr<std::function<void()>>(action));
    }

    void transaction::last(const std::function<void()>& action) {
        impl->last_q(new std::shared_ptr<std::function<void()>>(action));
    }

}  // end namespace SODIUM_NAMESPACE

