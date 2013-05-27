/**
 * Copyright (c) 2012, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM_COUNT_SET_H_
#define _SODIUM_COUNT_SET_H_

#include <limits.h>
#include <assert.h>

namespace sodium {
    namespace impl {
        
        struct large_count_set {
            large_count_set(
                    unsigned strong_count,
                    unsigned event_count,
                    unsigned node_count
                ) : strong_count(strong_count),
                    event_count(event_count),
                    node_count(node_count)
            {
            }
            unsigned strong_count;
            unsigned event_count;
            unsigned node_count;
        };

        #if UINTPTR_MAX <= 0xffffffffu
        #define SODIUM_STRONG_BITS 1
        #define SODIUM_EVENT_BITS  14
        #define SODIUM_NODE_BITS   14
        #elif UINTPTR_MAX <= 0xffffffffffffffffu
        #define SODIUM_STRONG_BITS 1
        #define SODIUM_EVENT_BITS  31
        #define SODIUM_NODE_BITS   31
        #else
        #error This architecture is not supported
        #endif

        struct small_count_set {
            unsigned is_small:1;
            unsigned strong_count:SODIUM_STRONG_BITS;
            unsigned event_count:SODIUM_EVENT_BITS;
            unsigned node_count:SODIUM_NODE_BITS;
        };
        
        #define SODIUM_STRONG_MAX ((1u << SODIUM_STRONG_BITS) - 1u)
        #define SODIUM_EVENT_MAX  ((1u << SODIUM_EVENT_BITS) - 1u)
        #define SODIUM_NODE_MAX   ((1u << SODIUM_NODE_BITS) - 1u)

        union count_set_impl {
            small_count_set small;
            large_count_set* large;
        };

        /*!
         * Three counters implemented so as to fit into one machine word in the common case.
         */
        class count_set {
            private:
                count_set(const count_set& other) {}
                count_set& operator = (const count_set& other) { return *this; }
                count_set_impl impl;
                void to_large() {
                    impl.large = new large_count_set(impl.small.strong_count, impl.small.event_count, impl.small.node_count);
                    assert(!impl.small.is_small);
                }
            public:
                count_set() {
                    impl.small.strong_count = 0;
                    impl.small.event_count = 0;
                    impl.small.node_count = 0;
                    impl.small.is_small = 1;
                }
                ~count_set() {
                    if (!impl.small.is_small)
                        delete impl.large;
                }
                bool active() const {
                    return impl.small.is_small ? impl.small.strong_count || (impl.small.node_count && impl.small.event_count)
                                               : impl.large->strong_count || (impl.large->node_count && impl.large->event_count);
                }
                bool alive() const {
                    return impl.small.is_small ? impl.small.strong_count || impl.small.node_count || impl.small.event_count
                                         : impl.large->strong_count || impl.large->node_count || impl.large->event_count;
                }
                void inc_strong() {
                    if (impl.small.is_small) {
                        if (impl.small.strong_count == SODIUM_STRONG_MAX)
                            to_large();
                        else {
                            impl.small.strong_count++;
                            return;
                        }
                    }
                    impl.large->strong_count++;
                }
                void dec_strong() {
                    if (impl.small.is_small)
                        impl.small.strong_count--;
                    else
                        impl.large->strong_count--;
                }
                void inc_event() {
                    if (impl.small.is_small) {
                        if (impl.small.event_count == SODIUM_EVENT_MAX)
                            to_large();
                        else {
                            impl.small.event_count++;
                            return;
                        }
                    }
                    impl.large->event_count++;
                }
                void dec_event() {
                    if (impl.small.is_small)
                        impl.small.event_count--;
                    else
                        impl.large->event_count--;
                }
                void inc_node() {
                    if (impl.small.is_small) {
                        if (impl.small.node_count == SODIUM_NODE_MAX)
                            to_large();
                        else {
                            impl.small.node_count++;
                            return;
                        }
                    }
                    impl.large->node_count++;
                }
                void dec_node() {
                    if (impl.small.is_small)
                        impl.small.node_count--;
                    else
                        impl.large->node_count--;
                }
        };
    }
}
#endif

