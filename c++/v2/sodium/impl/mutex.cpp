#include "mutex.h"

namespace sodium {
    namespace impl {
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
    }
}
