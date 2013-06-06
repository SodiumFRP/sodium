/**
 * Copyright (c) 2012-2013, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM_LOCKPOOL_H_
#define _SODIUM_LOCKPOOL_H_

#include <pthread.h>
#include <stdint.h>
#include <limits.h>

namespace sodium {
    namespace impl {
        struct spin_lock {
            pthread_spinlock_t sl;
            spin_lock() {
                pthread_spin_init(&sl, PTHREAD_PROCESS_PRIVATE);
            }
            inline void lock() {
                pthread_spin_lock(&sl);
            }
            inline void unlock() {
                pthread_spin_unlock(&sl);
            }
        };
        #define SODIUM_IMPL_LOCK_POOL_BITS 7
        extern spin_lock lock_pool[1<<SODIUM_IMPL_LOCK_POOL_BITS];

        // Use Knuth's integer hash function ("The Art of Computer Programming", section 6.4)
        inline spin_lock* spin_get_and_lock(void* addr)
        {
            spin_lock* l = &lock_pool[(uint32_t)((uint32_t)
        #if __WORDSIZE == 32
                (addr)
        #elif __WORDSIZE == 64
                (uint64_t)(addr)
        #else
        #error This architecture is not supported
        #endif
                * (uint32_t)2654435761U) >> (32 - SODIUM_IMPL_LOCK_POOL_BITS)];
            l->lock();
            return l;
        }
    }
}

#endif

