#ifndef _SODIUM_IMPL_MAGIC_REF_H_
#define _SODIUM_IMPL_MAGIC_REF_H_

#include <sodium/impl/mutex.h>
#include <list>
#include <set>
#include <boost/optional.hpp>
#include <stdlib.h>
#include <stdint.h>

namespace sodium {
    namespace impl {

        /* Bacon/Attanasio/Rajan/Smith algorithm. */
        struct link {
            enum colour_t { black, grey, white, purple };
            link() : ref_count(1), colour(colour_t::black) {}
            virtual ~link() {}
            uint32_t ref_count : 29;
            colour_t colour : 3;
            std::list<link*> children;  // Other links that we reference
            static std::list<link*>* capturer;
            static mutex lock;
            static std::set<link*> roots;
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
                r->decrement();
                r = other.r;
                r->increment();
                link::lock.unlock();
            }

            void assign(const A& a) const {
                link::lock.lock();
                link::capturer = &r->children;
                r->oa = boost::optional<A>(a);
                link::capturer = NULL;
                link::lock.unlock();
            }

            link* l() const { return r; }

            inline A* operator ->() { return &r->oa.get(); }
            inline A* operator ->() const { return &r->oa.get(); }

            inline A& operator *() { return r->oa.get(); }
            inline A& operator *() const { return r->oa.get(); }

            inline A* get() { return &r->oa.get(); }
            inline A* get() const { return &r->oa.get(); }
        };
    }

    void collect_cycles();
}

#endif

