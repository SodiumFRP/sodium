#include <sodium/sodium.h>

using namespace std;
using namespace boost;
using namespace heist;

namespace sodium {

    namespace impl {

        void node::link(void* handler, const std::shared_ptr<node>& target) {
            if (target) {
                std::set<node*> visited;
                target->ensure_bigger_than(visited, rank);
            }
            targets.push_back(target(handler, target));
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
                    if (it->target)
                        it->target->ensure_bigger_than(visited, rank);
            }
        }

        unsigned long long rankOf(const std::shared_ptr<node>& target)
        {
            if (target.get() != NULL)
                return target->rank;
            else
                return ULLONG_MAX;
        }

        static const std::shared_ptr<partition_state>& instance()
        {
            static std::shared_ptr<partition_state> st;
            // to do: make it properly thread-safe
            if (!st)
                st = shared_ptr<partition_state>(new partition_state());
            return st;
        }

        partition_state::partition_state()
            : nextlistenerID(0)
        {
            pthread_mutexattr_t attr;
            pthread_mutexattr_init(&attr);
            pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
            pthread_mutex_init(&transaction_lock, &attr);
            pthread_mutex_init(&listeners_lock, &attr);
        }

        partition_state::~partition_state()
        {
            pthread_mutex_destroy(&listeners_lock);
            pthread_mutex_destroy(&transaction_lock);
        }

        nodeID partition_state::allocNodeID()
        {
            nodeID id = nextnodeID;
            nextnodeID = nextnodeID.succ();
            return id;
        }
        
        transaction_impl* transaction::current_transaction;
        
        transaction_impl::state()
            : to_regen(false)
        {
        }

        transaction_impl::check_regen() {
            if (to_regen) {
                to_regen = false;
                prioritizedQ.clear();
                for (auto it = entries.begin(); it != entries.end(); ++it)
                    prioritizedQ.insert(pair<unsigned long long, entryID>(rankOf(it->second.target), it->first);
            }
        }

        transaction_impl::~state()
        {
            while (true) {
                check_regen();
                auto pit = prioritizedQ.begin();
                if (pit == prioritizedQ.end()) break;
                auto eit = entries.find(pit.second);
                assert(eit != entries.end());
                std::function<void(transaction*)> action = eit->second.action;
                entries.remove(eit);
                action.run(this);
            }
            while (lastQ.begin() != lastQ.end())
                (*lastQ.begin())();
                lastQ.erase(lastQ.begin());
            }
            while (postQ.begin() != postQ.end())
                (*postQ.begin())();
                postQ.erase(postQ.begin());
            }
        }

        void transaction_impl::prioritized(const std::shared_ptr<node>& target, const std::function<void(transaction*)>& f)
        {
            entryID id = nextEntryID;
            nextEntryID = nextEntryID.succ();
            Entry entry(target, f);
            entries.insert(id, entry);
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

    };  // end namespace impl

    transaction::transaction()
    {
        auto part = partition_state::instance();
        pthread_mutex_lock(&part->transaction_lock);
        transaction_was = current_transaction;
        if (current_transaction == null)
            current_transaction = new state;
    }

    transaction::~transaction()
    {
        if (transaction_was == null)
            delete current_transaction;
        current_transaction = transaction_was;
        auto part = partition_state::instance();
        pthrad_mutex_unlock(&part->transaction_lock);
    }

};  // end namespace sodium

