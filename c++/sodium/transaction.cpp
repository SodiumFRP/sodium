/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#include <sodium/sodium.h>
#include <limits.h>

using namespace std;
using namespace boost;

namespace sodium {

    mutex::mutex()
    {
        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
        pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
        pthread_mutex_init(&mx, &attr);
    }
    
    mutex::~mutex()
    {
        pthread_mutex_destroy(&mx);
    }

    partition::partition()
        : depth(0)
    {
        pthread_key_create(&key, NULL);
    }

    partition::~partition()
    {
        pthread_key_delete(key);
    }

    partition* def_part::part()
    {
        static partition part;
        return &part;
    }

    namespace impl {

        void node::link(void* handler, const std::shared_ptr<node>& targ) {
            if (targ) {
                std::set<node*> visited;
                targ->ensure_bigger_than(visited, rank);
            }
            targets.push_back(target(handler, targ));
        }

        bool node::unlink(void* handler) {
            for (auto it = targets.begin(); it != targets.end(); ++it)
                if (it->handler == handler) {
                    targets.erase(it);
                    return true;
                }
            return false;
        }

        void node::ensure_bigger_than(std::set<node*>& visited, unsigned long long limit)
        {
            if (rank > limit || visited.find(this) != visited.end())
                ;
            else {
                visited.insert(this);
                rank = limit + 1;
                for (auto it = targets.begin(); it != targets.end(); ++it)
                    if (it->n)
                        it->n->ensure_bigger_than(visited, rank);
            }
        }

        unsigned long long rankOf(const std::shared_ptr<node>& target)
        {
            if (target.get() != NULL)
                return target->rank;
            else
                return ULLONG_MAX;
        }

        nodeID allocNodeID()
        {
            static nodeID nextnodeID;

            nodeID id = nextnodeID;
            nextnodeID = nextnodeID.succ();
            return id;
        }

        transaction_impl::transaction_impl(partition* part)
            : part(part),
              to_regen(false)
        {
        }

        void transaction_impl::check_regen() {
            if (to_regen) {
                to_regen = false;
                prioritizedQ.clear();
                for (auto it = entries.begin(); it != entries.end(); ++it)
                    prioritizedQ.insert(pair<unsigned long long, entryID>(rankOf(it->second.target), it->first));
            }
        }

        transaction_impl::~transaction_impl()
        {
        }
        
        void transaction_impl::process_transactional()
        {
            while (true) {
                check_regen();
                auto pit = prioritizedQ.begin();
                if (pit == prioritizedQ.end()) break;
                auto eit = entries.find(pit->second);
                assert(eit != entries.end());
                std::function<void(transaction_impl*)> action = eit->second.action;
                prioritizedQ.erase(pit);
                entries.erase(eit);
                action(this);
            }
            while (lastQ.begin() != lastQ.end()) {
                (*lastQ.begin())();
                lastQ.erase(lastQ.begin());
            }
        }

        void transaction_impl::process_post()
        {
            while (postQ.begin() != postQ.end()) {
                (*postQ.begin())();
                postQ.erase(postQ.begin());
            }
        }

        void transaction_impl::prioritized(const std::shared_ptr<node>& target,
                                           const std::function<void(transaction_impl*)>& f)
        {
            entryID id = next_entry_id;
            next_entry_id = next_entry_id.succ();
            entries.insert(pair<entryID, prioritized_entry>(id, prioritized_entry(target, f)));
            prioritizedQ.insert(pair<unsigned long long, entryID>(rankOf(target), id));
        }
    
        void transaction_impl::last(const std::function<void()>& action)
        {
            lastQ.push_back(action);
        }

        void transaction_impl::post(const std::function<void()>& action)
        {
            postQ.push_back(action);
        }

        transaction_::transaction_(partition* part)
            : impl_(policy::get_global()->current_transaction(part))
        {
            if (impl_ == NULL) {
                impl_ = new transaction_impl(part);
                policy::get_global()->initiate(impl_);
            }
            part->depth++;  // note: transaction is locked here, giving us exclusive access to part
        }

        transaction_::~transaction_()
        {
            partition* part = impl_->part;
            part->depth--;  // note: transaction is locked here, giving us exclusive access to part
            if (part->depth == 0) {
                impl::transaction_impl* impl_(this->impl_);
                policy::get_global()->dispatch(
                    impl_,
                    [impl_] () {
                        impl_->process_transactional();
                    },
                    [impl_] () {
                        impl_->process_post();
                        delete impl_;
                    }
                );
            }
        }
    };  // end namespace impl

    static policy* global_policy = new simple_policy;

    /*static*/ policy* policy::get_global()
    {
        return global_policy;
    }

    /*static*/ void policy::set_global(policy* policy)
    {
        delete global_policy;
        global_policy = policy;
    }

    simple_policy::simple_policy()
    {
    }
    
    simple_policy::~simple_policy()
    {
    }

    impl::transaction_impl* simple_policy::current_transaction(partition* part)
    {
        return static_cast<impl::transaction_impl*>(pthread_getspecific(part->key));
    }

    void simple_policy::initiate(impl::transaction_impl* impl)
    {
        impl->part->mx.lock();
        pthread_setspecific(impl->part->key, impl);
    }

    void simple_policy::dispatch(impl::transaction_impl* impl,
        const std::function<void()>& transactional,
        const std::function<void()>& post)
    {
        partition* part = impl->part;
        transactional();
        pthread_setspecific(part->key, NULL);
        post();  // note: deletes 'impl'
        part->mx.unlock();
    }

};  // end namespace sodium

