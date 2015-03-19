/**
 * Copyright (c) 2012-2015, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#ifndef _SODIUM2_IMPL_MAGIC_REF_H_
#define _SODIUM2_IMPL_MAGIC_REF_H_

#include <sodium/impl/common.h>
#include <sodium/impl/mutex.h>
#include <list>
#include <set>
#include <boost/optional.hpp>
#include <stdlib.h>
#include <stdint.h>

namespace SODIUM_NAMESPACE {
    namespace impl {

        /* Bacon/Attanasio/Rajan/Smith algorithm. */
        struct link {
            enum colour_t { black, grey, white, purple };
            link() : ref_count(1), colour(colour_t::black), buffered(false) {
            }
            virtual ~link() {}
            uint32_t ref_count : 28;
            colour_t colour : 3;
            bool buffered : 1;
            std::list<link*> children;  // Other links that we reference
            static std::list<link*>* capturer;
            static mutex lock;
            static std::set<link*> roots;
            static std::set<link*> to_delete;
            void increment();
            void decrement();
            void release();
            void possible_root();
            static void collect_cycles();
            static void mark_roots();
            static void scan_roots();
            static void collect_roots();
            void mark_grey();
            void scan();
            void scan_black();
            void collect_white();
            void dump_white();
            void capturer_acquire();
            void capturer_release();
        };

        template <class A>
        struct receptacle : link {
            receptacle() {}
            receptacle(const A& a) : oa(a) {}
            virtual ~receptacle() {}
            boost::optional<A> oa;
        };

        template <class A>
        class magic_ref {
        private:
            receptacle<A>* r;
        public:
            magic_ref() : r(new receptacle<A>) {}
            magic_ref(const A& a) : r(new receptacle<A>) {
                assign(a);
            }
            magic_ref(const magic_ref<A>& other) {
                link::lock.lock();
                r = other.r;
                r->increment();
                link::lock.unlock();
            }
            ~magic_ref() {
                link::lock.lock();
                r->decrement();
                link::lock.unlock();
            }

            const std::list<link*>& children() const { return r->children; }

            magic_ref<A>& operator = (const magic_ref<A>& other) {
                link::lock.lock();
                if (r != other.r) {
                    r->decrement();
                    r = other.r;
                    r->increment();
                }
                link::lock.unlock();
                return *this;
            }

            void assign(const A& a) const {
                link::lock.lock();
                r->children.clear();
                r->oa = boost::optional<A>();
                std::list<link*>* capturer_was = link::capturer;
                link::capturer = &r->children;
                r->oa = boost::optional<A>(a);
                link::capturer = capturer_was;
                link::lock.unlock();
            }

            void reset() const {
                link::lock.lock();
                r->children.clear();
                r->oa = boost::optional<A>();
                link::lock.unlock();
            }

            /*!
             * Return true if this reference has been assign()ed.
             */
            operator bool () const {
                return (bool)r->oa; 
            }

            link* l() const { return r; }

            // The returned pointers are const because the only safe way to
            // modify is through 'assign'.
            inline const A* operator ->() const { return &r->oa.get(); }
            inline const A& operator *() const { return r->oa.get(); }
            inline const A& get() const { return r->oa.get(); }
            inline const A* get_ptr() const { return &r->oa.get(); }
            /*!
             * Variant of get() that allows you to modify the contents in-place.
             * This MUST NOT change any contained magic_refs. This will break the
             * reference tracking.
             * If you wish to do this, use assign().
             */
            inline A& unsafe_get() const { return r->oa.get(); }
        };
    }

    void collect_cycles();
}

#endif

