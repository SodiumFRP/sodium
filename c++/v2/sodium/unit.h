/**
 * Copyright (c) 2012-2015, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM2_UNIT_H_
#define _SODIUM2_UNIT_H_

#include <sodium/impl/common.h>

namespace SODIUM_NAMESPACE {
    /*!
     * A unit (or "nothing") type and value.
     */
    struct unit {
        unit() {}
        bool operator == (const unit& other) const { return true; }
        bool operator != (const unit& other) const { return false; }
    };
};

#endif

