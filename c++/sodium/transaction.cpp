/**
 * Copyright (c) 2012-2013, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#include <sodium/sodium.h>
#include <limits.h>

using namespace std;
using namespace boost;

namespace sodium {

    namespace impl {
        
        void intrusive_ptr_add_ref(sodium::impl::listen_impl_func<sodium::impl::H_EVENT>* p)
        {
            spin_lock* l = spin_get_and_lock(p);
            p->counts.inc_event();
            l->unlock();
        }

        void intrusive_ptr_release(sodium::impl::listen_impl_func<sodium::impl::H_EVENT>* p)
        {
            spin_lock* l = spin_get_and_lock(p);
            p->counts.dec_event();
            p->update_and_unlock(l);
        }

        void intrusive_ptr_add_ref(sodium::impl::listen_impl_func<sodium::impl::H_STRONG>* p)
        {
            spin_lock* l = spin_get_and_lock(p);
            p->counts.inc_strong();
            l->unlock();
        }
        
        void intrusive_ptr_release(sodium::impl::listen_impl_func<sodium::impl::H_STRONG>* p)
        {
            spin_lock* l = spin_get_and_lock(p);
            p->counts.dec_strong();
            p->update_and_unlock(l);
        }

        void intrusive_ptr_add_ref(sodium::impl::listen_impl_func<sodium::impl::H_NODE>* p)
        {
            spin_lock* l = spin_get_and_lock(p);
            p->counts.inc_node();
            l->unlock();
        }

        void intrusive_ptr_release(sodium::impl::listen_impl_func<sodium::impl::H_NODE>* p)
        {
            spin_lock* l = spin_get_and_lock(p);
            p->counts.dec_node();
            p->update_and_unlock(l);
        }
    }

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
        : depth(0),
          processing_post(false)
    {
        pthread_key_create(&key, NULL);
    }

    partition::~partition()
    {
        pthread_key_delete(key);
    }

    void partition::post(const std::function<void()>& action)
    {
        mx.lock();
        postQ.push_back(action);
        mx.unlock();
    }

    void partition::process_post()
    {
        mx.lock();
        // Prevent it running on multiple threads at the same time, so posts
        // will be handled in order for the partition.
        if (!processing_post) {
            processing_post = true;
            try {
                while (postQ.begin() != postQ.end()) {
                    std::function<void()> action = *postQ.begin();
                    postQ.erase(postQ.begin());
                    mx.unlock();
                    action();
                    mx.lock();
                }
                processing_post = false;
            }
            catch (...) {
                processing_post = false;
                throw;
            }
        }
        mx.unlock();
    }

    partition* def_part::part()
    {
        static partition part;
        return &part;
    }

    namespace impl {

        node::node() : rank(0) {}
        node::node(rank_t rank) : rank(rank) {}
        node::~node()
        {
            for (auto it = targets.begin(); it != targets.end(); it++) {
                auto targ = it->n;
                if (targ) {
                    boost::intrusive_ptr<listen_impl_func<H_EVENT>> li(
                        reinterpret_cast<listen_impl_func<H_EVENT>*>(listen_impl.get()));
                    targ->sources.remove(li);
                }
            }
        }

        void node::link(void* holder, const std::shared_ptr<node>& targ)
        {
            if (targ) {
                std::set<node*> visited;
                targ->ensure_bigger_than(visited, rank);
                boost::intrusive_ptr<listen_impl_func<H_EVENT>> li(
                    reinterpret_cast<listen_impl_func<H_EVENT>*>(listen_impl.get()));
                targ->sources.push_front(li);
            }
            targets.push_front(target(holder, targ));
        }

        void node::unlink(void* holder)
        {
            std::forward_list<node::target>::iterator this_it;
            for (auto last_it = targets.before_begin(); true; last_it = this_it) {
                this_it = last_it;
                ++this_it;
                if (this_it == targets.end())
                    break;
                if (this_it->h == holder) {
                    auto targ = this_it->n;
                    targets.erase_after(last_it);
                    if (targ) {
                        boost::intrusive_ptr<listen_impl_func<H_EVENT>> li(
                            reinterpret_cast<listen_impl_func<H_EVENT>*>(listen_impl.get()));
                        targ->sources.remove(li);
                    }
                    break;
                }
            }
        }

        void node::ensure_bigger_than(std::set<node*>& visited, rank_t limit)
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

        rank_t rankOf(const std::shared_ptr<node>& target)
        {
            if (target.get() != NULL)
                return target->rank;
            else
                return SODIUM_IMPL_RANK_T_MAX;
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
                    prioritizedQ.insert(pair<rank_t, entryID>(rankOf(it->second.target), it->first));
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

        void transaction_impl::prioritized(const std::shared_ptr<node>& target,
                                           const std::function<void(transaction_impl*)>& f)
        {
            entryID id = next_entry_id;
            next_entry_id = next_entry_id.succ();
            entries.insert(pair<entryID, prioritized_entry>(id, prioritized_entry(target, f)));
            prioritizedQ.insert(pair<rank_t, entryID>(rankOf(target), id));
        }
    
        void transaction_impl::last(const std::function<void()>& action)
        {
            lastQ.push_back(action);
        }

        transaction_::transaction_(partition* part)
            : impl_(policy::get_global()->current_transaction(part))
        {
            if (impl_ == NULL) {
                impl_ = new transaction_impl(part);
                policy::get_global()->initiate(impl_);
            }
            part->depth++;
        }

        transaction_::~transaction_()
        {
            partition* part = impl_->part;
            if (part->depth == 1) {
                impl::transaction_impl* impl_(this->impl_);
                policy::get_global()->dispatch(
                    impl_,
                    [impl_] () {
                        impl_->process_transactional();
                        impl_->part->depth--;
                    },
                    [impl_] () {
                        partition* part = impl_->part;
                        delete impl_;
                        part->process_post();
                    }
                );
            }
            else
                part->depth--;
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
        return reinterpret_cast<impl::transaction_impl*>(pthread_getspecific(part->key));
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
        transactional();
        pthread_setspecific(impl->part->key, NULL);
        impl->part->mx.unlock();
        post();  // note: deletes 'impl'
    }

};  // end namespace sodium

