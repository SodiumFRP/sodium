#ifndef _SODIUM_IMPL_MUTEX_H_
#define _SODIUM_IMPL_MUTEX_H_

#include <pthread.h>

namespace sodium {
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
