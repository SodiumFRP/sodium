/**
 * Copyright (c) 2012-2014, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM_LOCKPOOL_H_
#define _SODIUM_LOCKPOOL_H_

#include <sodium/config.h>

#ifdef __APPLE__
#include <libkern/OSAtomic.h>
#elif defined(__TI_COMPILER_VERSION__)
#else
#include <pthread.h>
#endif
#include <stdint.h>
#include <limits.h>

namespace sodium {
    namespace impl {
        struct spin_lock {
#if defined(SODIUM_SINGLE_THREADED)
            inline void lock() {}
            inline void unlock() {}
#elif defined(__APPLE__)
            OSSpinLock sl;
            spin_lock() : sl(OS_SPINLOCK_INIT) {
            }
            inline void lock() {
                OSSpinLockLock(&sl);
            }
            inline void unlock() {
                OSSpinLockUnlock(&sl);
            }
#else
            bool initialized;
            pthread_spinlock_t sl;
            spin_lock() : initialized(true) {
                pthread_spin_init(&sl, PTHREAD_PROCESS_PRIVATE);
            }
            inline void lock() {
                // Make sure nothing bad happens if this is called before the constructor.
                // This can happen during static initialization if data structures that use
                // this lock pool are declared statically.
                if (initialized) pthread_spin_lock(&sl);
            }
            inline void unlock() {
                if (initialized) pthread_spin_unlock(&sl);
            }
#endif
        };
		#if defined(SODIUM_SINGLE_THREADED)
			#define SODIUM_IMPL_LOCK_POOL_BITS 1
		#else
			#define SODIUM_IMPL_LOCK_POOL_BITS 7
        #endif
        extern spin_lock lock_pool[1<<SODIUM_IMPL_LOCK_POOL_BITS];

        // Use Knuth's integer hash function ("The Art of Computer Programming", section 6.4)
        inline spin_lock* spin_get_and_lock(void* addr)
        {
#if defined(SODIUM_SINGLE_THREADED)
        	return &lock_pool[0];
#else
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
#endif
        }
    }
}

#endif

