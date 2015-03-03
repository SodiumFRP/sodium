/**
 * Copyright (c) 2012-2015, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
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
