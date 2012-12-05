#include <sodium/light_ptr.h>
#include <pthread.h>

namespace sodium {
    static pthread_spinlock_t* get_spinlock()
    {
        static pthread_spinlock_t* gs;
        if (gs == NULL) {
            gs = new pthread_spinlock_t;
            pthread_spin_init(gs, PTHREAD_PROCESS_PRIVATE);
        }
        return gs;
    }

#define SODIUM_DEFINE_LIGHTPTR(Name, GET_LOCK, LOCK, UNLOCK) \
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
        GET_LOCK; \
        LOCK; \
        count->count++; \
        UNLOCK; \
    } \
     \
    Name::~Name() { \
        GET_LOCK; \
        LOCK; \
        if (count != NULL && --count->count == 0) { \
            UNLOCK; \
            count->del(value); delete count; \
        } \
        else { \
            UNLOCK; \
        } \
    } \
     \
    Name& Name::operator = (const Name& other) { \
        GET_LOCK; \
        LOCK; \
        if (--count->count == 0) { \
            UNLOCK; \
            count->del(value); delete count; \
        } \
        else { \
            UNLOCK; \
        } \
        value = other.value; \
        count = other.count; \
        LOCK; \
        count->count++; \
        UNLOCK; \
        return *this; \
    }

SODIUM_DEFINE_LIGHTPTR(light_ptr, pthread_spinlock_t* s = get_spinlock(),
                          pthread_spin_lock(s),
                          pthread_spin_unlock(s))

SODIUM_DEFINE_LIGHTPTR(unsafe_light_ptr,,,)

};

