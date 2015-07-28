/**
 * Copyright (c) 2012-2014, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM_CONFIG_H_
#define _SODIUM_CONFIG_H_

#include <limits.h>  // for __WORDSIZE

#if defined(__MSP430__)
#define SODIUM_NO_CXX11
#define SODIUM_NO_EXCEPTIONS
#define SODIUM_SINGLE_THREADED
#endif

#if defined(SODIUM_NO_CXX11)
#include <boost/shared_ptr.hpp>
#endif

#if defined(__MSP430__)
/*
#define SODIUM_STRONG_BITS 1
#define SODIUM_EVENT_BITS  6
#define SODIUM_NODE_BITS   6
#define SODIUM_CONSERVE_MEMORY
*/
#elif __WORDSIZE == 32
#define SODIUM_STRONG_BITS 1
#define SODIUM_EVENT_BITS  14
#define SODIUM_NODE_BITS   14
#define SODIUM_CONSERVE_MEMORY
#elif __WORDSIZE == 64
#define SODIUM_STRONG_BITS 1
#define SODIUM_EVENT_BITS  31
#define SODIUM_NODE_BITS   31
#define SODIUM_CONSERVE_MEMORY
#endif

#if defined(SODIUM_NO_CXX11)
#define SODIUM_SHARED_PTR   boost::shared_ptr
namespace sodium {
    template <class T>
    boost::shared_ptr<T> make_shared()
    {
        return boost::shared_ptr<T>(new T);
    }
}
#define SODIUM_MAKE_SHARED  sodium::make_shared
#define SODIUM_WEAK_PTR     boost::weak_ptr
#define SODIUM_TUPLE        boost::tuple
#define SODIUM_MAKE_TUPLE   boost::make_tuple
#define SODIUM_TUPLE_GET    boost::get
#define SODIUM_FORWARD_LIST std::list
#else
#define SODIUM_SHARED_PTR   std::shared_ptr
#define SODIUM_MAKE_SHARED  std::make_shared
#define SODIUM_WEAK_PTR     std::weak_ptr
#define SODIUM_TUPLE        std::tuple
#define SODIUM_MAKE_TUPLE   std::make_tuple
#define SODIUM_TUPLE_GET    std::get
#define SODIUM_FORWARD_LIST std::forward_list
#endif

#endif
