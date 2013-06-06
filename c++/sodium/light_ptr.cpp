/**
 * Copyright (c) 2012-2013, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#include <sodium/light_ptr.h>
#include <sodium/lock_pool.h>
#include <pthread.h>

namespace sodium {
#define SODIUM_DEFINE_LIGHTPTR(Name, GET_AND_LOCK, UNLOCK) \
    Name::Name() \
        : value(NULL), count(NULL) \
    { \
    } \
     \
    Name Name::DUMMY; \
     \
    Name::Name(void* value, impl::deleter del) \
        : value(value), count(new impl::count(1, del)) \
    { \
    } \
     \
    Name::Name(const Name& other) \
        : value(other.value), count(other.count) \
    { \
        GET_AND_LOCK; \
        count->c++; \
        UNLOCK; \
    } \
     \
    Name::~Name() { \
        GET_AND_LOCK; \
        if (count != NULL && --count->c == 0) { \
            UNLOCK; \
            count->del(value); delete count; \
        } \
        else { \
            UNLOCK; \
        } \
    } \
     \
    Name& Name::operator = (const Name& other) { \
        { \
            GET_AND_LOCK; \
            if (--count->c == 0) { \
                UNLOCK; \
                count->del(value); delete count; \
            } \
            else { \
                UNLOCK; \
            } \
        } \
        value = other.value; \
        count = other.count; \
        { \
            GET_AND_LOCK; \
            count->c++; \
            UNLOCK; \
        } \
        return *this; \
    }

SODIUM_DEFINE_LIGHTPTR(light_ptr, impl::spin_lock* l = impl::spin_get_and_lock(this->value),
                          l->unlock())

SODIUM_DEFINE_LIGHTPTR(unsafe_light_ptr,,)

};

