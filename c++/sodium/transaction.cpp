#include <sodium/sodium.h>

using namespace std;
using namespace boost;
using namespace heist;

namespace sodium {

// ------ partition -----------------------------------------------------------

    namespace impl {

        partition_state::partition_state(pthread_mutex_t* mutex)
        : mutex(mutex), nextlistenerID(0), recursive(false)
        {
        }

        partition_state::~partition_state()
        {
        }
    
        nodeID partition_state::allocnodeID()
        {
            nodeID id = nextnodeID;
            nextnodeID = nextnodeID.succ();
            return id;
        }

        void partition_state::opentransaction(long long id, transaction_impl* impl)
        {
            MutexLock ml(mutex);
            if (tracer.tracing(DIAG)) {
                string bt = backtraceToString(backtraceFetch());
                tracer.ptracef(DIAG, "open trans %lld: %s", id, bt.c_str());
            }
            activetransactions.insert(id);
        }

        void partition_state::closetransaction(long long id)
        {
            MutexLock ml(mutex);
            if (tracer.tracing(DIAG))
                tracer.ptracef(DIAG, "close trans %lld", id);
            auto it = activetransactions.find(id);
            if (it != activetransactions.end())
                activetransactions.erase(it);
            check_when_all_previous_();
        }

        /*!
         * Perform the specified action when we are sure that all transactions prior to
         * the specified one have been committed. Will execute now if this condition is
         * true now.
         *
         * For a given transaction id, callbacks are called in the order in which
         * when_all_previous_committed was called.
         */
        void partition_state::when_all_previous_committed(long long id, const std::function<void()>& f)
        {
            MutexLock ml(mutex);
            auto it = whenAllPreviouses.find(id);
            if (it == whenAllPreviouses.end())
                whenAllPreviouses[id] = { f };
            else
                it->second.push_back(f);
            check_when_all_previous_();
        }

        long long partition_state::oldest_active_transaction() const
        {
            MutexLock ml(const_cast<partition_state*>(this)->mutex);
            return oldest_active_transaction_();
        }

        long long partition_state::oldest_active_transaction_() const  // Call with mutex locked
        {
            long long oldest_active_transaction = LLONG_MAX;
            {
                auto it = activetransactions.begin();
                if (it != activetransactions.end())
                    oldest_active_transaction = *it;
            }
            return oldest_active_transaction;
        }

        void partition_state::check_when_all_previous_()  // Call with mutex locked
        {
            if (!recursive) {
                recursive = true;
                try {
                    while (true) {
                        long long oldest_active_transaction = this->oldest_active_transaction_();
                        // Examine the first "when all previous committed" target.
                        auto it = whenAllPreviouses.begin();
                        if (it == whenAllPreviouses.end()) break;
                        // If there are no active transactions less than this target
                        if (! (oldest_active_transaction < it->first)) {
                            // then pop and execute it
                            std::list<std::function<void()>>& fs = it->second;
                            sch::mkevent<void>(seq, [fs] () {
                                for (auto it2 = fs.begin(); it2 != fs.end(); ++it2)
                                    (*it2)();
                            })->schedule();
                            whenAllPreviouses.erase(it);
                            // repeat
                        }
                        else
                            break;
                    }
                    recursive = false;
                }
                catch (...) {
                    recursive = false;
                    throw;
                }
            }
        }
    }; // end namespace impl

// ------ transaction_impl -----------------------------------------------

    namespace impl {
        Mutex globaltransactionMutex;
        long long nexttransactionID;

        transaction_impl::transaction_impl(
            const std::shared_ptr<partition_state>& partitionSt,
            long long id_
        )
            : state(new State(partitionSt, id_))
        {
            if (partitionSt != NULL)
                partitionSt->opentransaction(id_, this);
        }

        transaction_impl::~transaction_impl() {
            if (state->partitionSt != NULL) {
                if (state->cleanups1.begin() != state->cleanups1.end()) {
                    std::function<void(const std::shared_ptr<transaction_impl>&)> cleanup = state->cleanups1.begin()->second;
                    state->cleanups1.erase(state->cleanups1.begin());
                    transaction_impl* impl = new transaction_impl(state); 
                    try {
                        sch::mkevent<void>(state->partitionSt->seq, [cleanup, impl] () {
                            try {
                                std::shared_ptr<transaction_impl> pImpl(impl);
                                cleanup(pImpl);
                            }
                            catch (const sch::SchedulerException& exc) {
                                // Discard scheduler exceptions, which can happen on executable
                                // shutdown when boost insists on cleaning up all shared pointers,
                                // after the scheduler is gone.
                                // TO DO: This is hardly a satisfactory solution. Find a proper one.
                                tracer.trace(ERR, string("While closing transaction (2): ")+exc.what());
                            }
                        })->schedule();
                    }
                    catch (const sch::SchedulerException& exc) {
                        // Discard scheduler exceptions, which can happen on executable
                        // shutdown when boost insists on cleaning up all shared pointers,
                        // after the scheduler is gone.
                        // TO DO: This is hardly a satisfactory solution. Find a proper one.
                        tracer.trace(ERR, string("While closing transaction (2): ")+exc.what());
                    }
                }
                else {
                    if (state->cleanups2.begin() == state->cleanups2.end()) {
                        state->partitionSt->closetransaction(state->id_);
                        delete state;
                    }
                    else {
                        State* state(this->state);
                        sch::mkevent<void>(state->partitionSt->seq, [state] () {
                            for (auto it = state->cleanups2.begin(); it != state->cleanups2.end(); ++it)
                                (*it)(state->id_);
                            state->partitionSt->closetransaction(state->id_);
                            delete state;
                        })->schedule();
                    }
                }
            }
            else
                delete state;
        }

        /*!
         * Perform the specified action on cleanup of this transaction, with the
         * transaction being resurrectable.
         */
        void transaction_impl::on_phase_1_cleanup(unsigned long long rank, const std::function<void(const std::shared_ptr<transaction_impl>&)>& f)
        {
            if (state->partitionSt == NULL) {
                tracer.trace(ERR, "transaction is NULL (1)");
                abort();
            }
            MutexLock ml(state->partitionSt->mutex);
            state->cleanups1.insert(pair<unsigned long long, std::function<void(const std::shared_ptr<transaction_impl>&)>>(rank, f));
        }

        /*!
         * Perform the specified action on cleanup of this transaction, after all
         * phase 1 cleanup has completed.
         */
        void transaction_impl::on_phase_2_cleanup(const std::function<void(long long)>& f)
        {
            if (state->partitionSt == NULL) {
                tracer.trace(ERR, "transaction is NULL (2)");
                abort();
            }
            MutexLock ml(state->partitionSt->mutex);
            on_phase_2_cleanup_(f);
        }
        void transaction_impl::on_phase_2_cleanup_(const std::function<void(long long)>& f)
        {
            state->cleanups2.push_back(f);
        }
    };  // end namespace impl

// ------ transaction ---------------------------------------------------------

};  // end namespace sodium

