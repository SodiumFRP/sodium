/**
 * Copyright (c) 2012-2013 Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
 
#include <sodium/lock_pool.h>


namespace sodium {
    namespace impl {
        spin_lock lock_pool[1<<SODIUM_IMPL_LOCK_POOL_BITS] __attribute__ ((init_priority (101)));
    }
}

