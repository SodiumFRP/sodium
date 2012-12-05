#ifndef _SODIUM_LIGHTPTR_H_
#define _SODIUM_LIGHTPTR_H_

namespace sodium {
    template <class A>
    void deleter(void* a0)
    {
        delete (A*)a0;
    }

    namespace impl {
        typedef void (*deleter)(void*);
        struct count {
            count(
                int count,
                deleter del
            ) : count(count), del(del) {}
            int count;
            deleter del;
        };
    };

    /*!
     * An untyped reference-counting smart pointer, thread-safe variant.
     */
    #define SODIUM_DECLARE_LIGHTPTR(name) \
        struct name { \
            static name DUMMY;  /* A null value that does not work, but can be used to */ \
                                    /* satisfy the compiler for unusable private constructors */ \
            name(); \
            name(const name& other); \
            template <class A> static inline name create(const A& a) { \
                return name(new A(a), deleter<A>); \
            } \
            name(void* value, impl::deleter del); \
            ~name(); \
            name& operator = (const name& other); \
            void* value; \
            impl::count* count; \
         \
            template <class A> inline A* castPtr(A*) {return (A*)value;} \
            template <class A> inline const A* castPtr(A*) const {return (A*)value;} \
        };

    SODIUM_DECLARE_LIGHTPTR(light_ptr)        // Thread-safe variant
    SODIUM_DECLARE_LIGHTPTR(unsafe_light_ptr)  // Non-thread-safe variant
};

#endif

