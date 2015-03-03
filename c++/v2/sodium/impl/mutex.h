/**
 * Copyright (c) 2012-2015, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM2_IMPL_MUTEX_H_
#define _SODIUM2_IMPL_MUTEX_H_

#include <sodium/impl/common.h>
#include <pthread.h>

namespace SODIUM_NAMESPACE {
    namespace impl {
        class mutex
        {
        private:
            pthread_mutex_t mx;
            // ensure we don't copy or assign a mutex by value
            mutex(const mutex& other) {}
            mutex& operator = (const mutex& other) { return *this; }
        public:
            mutex();
            ~mutex();
            void lock()
            {
                pthread_mutex_lock(&mx);
            }
            void unlock()
            {
                pthread_mutex_unlock(&mx);
            }
        };
     }
}

#endif
