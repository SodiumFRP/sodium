#ifndef _SODIUM_TRANSACTION_H_
#define _SODIUM_TRANSACTION_H_

#include <boost/shared_ptr.hpp>
#include <sodium/unit.h>
#include <pthread.h>

namespace sodium {

    namespace impl {
        struct nodeID {
            nodeID() : id(0) {}
            nodeID(unsigned long long id) : id(id) {}
            unsigned long long id;
            nodeID succ() const { return nodeID(id+1); }
            inline bool operator < (const nodeID& other) const { return id < other.id; }
        };

        class transaction_impl;
        class partition_state {
            public:
                pthread_mutex_t* mutex;

                partition_state(pthread_mutex_t* mutex);
                ~partition_state();

                nodeID allocnodeID();
                void opentransaction(long long id, impl::transaction_impl* impl);
                void closetransaction(long long id);
                /* Caller must call on this partition's sequence. */
                void when_all_previous_committed(long long id, const std::function<void()>& f);
                void dumpOpen(std::ostream& os) const;
                long long oldest_active_transaction() const;

            private:
                nodeID nextnodeID;
                std::set<long long> activetransactions;
                int nextlistenerID;
                std::map<long long, std::list<std::function<void()>>> whenAllPreviouses;
                bool recursive;
                long long oldest_active_transaction_() const;  // Call with mutex locked
                void check_when_all_previous_();  // Call with mutex locked
        };

        extern long long nexttransactionID;
    };

    template <class P>
    class transaction;

    template <class P>
    class partition {
        friend class impl::transaction_impl;
        friend class frp::transaction<P>;
            std::shared_ptr<impl::partition_state> state;

        private:
            partition() {}  // Use partition::create() instead because we don't want this
                            // called accidentally by omitting to copy a partition over.
            partition(std::shared_ptr<impl::partition_state> state) : state(state) {}

        public:
            // Copy constructor and == - default implementations
            static partition<P> create(pthread_mutex_t* mutex)
            {
                return partition(std::shared_ptr<impl::partition_state>(new impl::partition_state(mutex)));
            }

            // Dummy partition to be assigned later. Use with care.
            static partition<P> createNULL()
            {
                return partition(std::shared_ptr<impl::partition_state>());
            }

            bool operator == (const partition& other) const { return state.get() == other.state.get(); }
            bool operator != (const partition& other) const { return state.get() != other.state.get(); }

            long long oldest_active_transaction() const
            {
                return state->oldest_active_transaction();
            }

            /*!
             * Perform the specified action when we are sure that all transactions prior to
             * the specified one have been committed. Will execute now if this condition is
             * true now.
             *
             * For a given transaction id, callbacks are called in the order in which
             * when_all_previous_committed was called.
             */
            void when_all_previous_committed(long long id, const std::function<void()>& f)
            {
                state->when_all_previous_committed(id, f);
            }

            pthread_mutex_t* mutex() const {
                return state->mutex;
            }

            impl::nodeID _impl_alloc_node_ID() { return state->allocnodeID(); }
    };

    namespace impl {
        class transaction_impl
        {
            template <class P>
            friend class frp::transaction;

            private:
                struct state {
                    state(
                        std::shared_ptr<impl::partition_state> partitionSt,
                        long long id_
                    )
                    : partitionSt(partitionSt), id_(id_) {}
                    std::shared_ptr<impl::partition_state> partitionSt;
                    long long id_;
                    std::multimap<unsigned long long, std::function<void(const std::shared_ptr<transaction_impl>&)>> cleanups1;
                    std::list<std::function<void(long long)>> cleanups2;
                };
                state* state;

                /*!
                 * Perform the specified action on cleanup of this transaction, with the
                 * transaction being resurrectable. Function is executed in the partition's
                 * sequence.
                 */
                void on_phase_1_cleanup(unsigned long long rank, const std::function<void(const std::shared_ptr<transaction_impl>&)>& cleanup);

                /*!
                 * Perform the specified action on cleanup of this transaction, after all
                 * phase 1 cleanup has completed. Function is executed in the partition's
                 * sequence.
                 */
                void on_phase_2_cleanup(const std::function<void(long long)>& f);
                void on_phase_2_cleanup_(const std::function<void(long long)>& f);
    
            public:
                transaction_impl(
                    const std::shared_ptr<impl::partition_state>& partitionSt,
                    long long id_
                );
                transaction_impl(state* state): state(state) {}
                ~transaction_impl();

                long long id() const {return state->id_;}
        };
    };

    struct cleaner_upper
    {
        std::function<void()> f;

        cleaner_upper(const std::function<void()>& f) : f(f) {}
        ~cleaner_upper() { f(); }
    };

    template <class P>
    class transaction {
        private:
            std::shared_ptr<impl::transaction_impl> impl;

        public:
            transaction(const partition<P>& partition)
            {
                long long id;
                {
                    MutexLock ml(impl::globaltransactionMutex);
                    id = impl::nexttransactionID++;
                }
                impl = std::shared_ptr<impl::transaction_impl>(new impl::transaction_impl(partition.state, id));
            }

            transaction(const std::shared_ptr<impl::transaction_impl>& impl)
                : impl(impl)
            {
            }

            transaction(const transaction& other)
            : impl(other.impl)
            {
            }

            ~transaction()
            {
            }

            transaction& operator = (const transaction& other)
            {
                impl = other.impl;
                return *this;
            }

            long long id() const {return impl->state->id_;}

            partition<P> partition() const
            {
                return impl->state->partitionSt;
            }

            pthread_mutex_t* mutex() const
            {
                return impl->state->partitionSt->mutex;
            }

            /*!
             * Invalidate this transaction handle. When all copies of the transaction are
             * cleaned up or closed, the transaction terminates.
             */
            void close()
            {
                impl = std::shared_ptr<impl::transaction_impl>();
            }

            /*!
             * Perform the specified action on cleanup of this transaction, with the
             * transaction being resurrectable.
             */
            void on_phase_1_cleanup(unsigned long long rank, const std::function<void(const std::shared_ptr<impl::transaction_impl>&)>& cleanup) const
            {
                impl->on_phase_1_cleanup(rank, cleanup);
            }

            /*!
             * Perform the specified action on cleanup of this transaction, after all
             * phase 1 cleanup has completed.
             */
            void on_phase_2_cleanup(const std::function<void(long long)>& f) const
            {
                impl->on_phase_2_cleanup(f);
            }

            void when_all_previous_committed(const std::function<void()>& f) const
            {
                impl->state->partitionSt->when_all_previous_committed(id(), f);
            }

            std::shared_ptr<impl::transaction_impl> get_impl() const {return impl;}

            /*!
             * To allow for untyped transactions to be used internally.
             */
            template <class Q>
            const transaction<Q>& cast__(Q*) const {
                return *(const transaction<Q>*)this;
            }
    };
};  // end namespace sodium

#endif

