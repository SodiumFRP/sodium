/**
 * Copyright (c) 2012-2015, Stephen Blackheath and Anthony Jones
 * Released under a BSD3 licence.
 *
 * C++ implementation courtesy of International Telematics Ltd.
 */
#include <sodium/impl/magic_ref.h>

namespace sodium {
    namespace impl {
        std::list<link*>* link::capturer;
        mutex             link::lock;
        std::set<link*>   link::roots;
        std::set<link*>   link::to_delete;

        void link::increment() {
            capturer_acquire();
            ref_count++;
            colour = colour_t::black;
        }

        void link::decrement() {
            // If not already deleted
            if (to_delete.find(this) == to_delete.end()) {
                capturer_release();
                ref_count--;
                if (ref_count == 0)
                    release();
                else
                    possible_root();
            }
        }

        void link::release() {
            /*
            for (auto it = children.begin(); it != children.end(); ++it) {
                link* l = *it;
                l->decrement();
            }
            */
            //colour = colour_t::black;  // @@@
            if (buffered)                // @@@
                roots.erase(this);       // @@@
            //if (!buffered)             // @@@
            printf("release %p\n", this);  // ###
            delete this;
        }

        void link::possible_root() {
            if (colour != colour_t::purple) {
                colour = colour_t::purple;
                if (!buffered) {
                    buffered = true;
                    roots.insert(this);
                }
            }
        }

        /*static*/ void link::collect_cycles() {
            mark_roots();
            scan_roots();
            collect_roots();
            for (auto it = to_delete.begin(); it != to_delete.end(); ++it) {
                link* l = *it;
                delete l;
            }
            to_delete.clear();
            // Cleaned up cycles should not be able to change anything outside
            // of themselves. If they can, then it means we have made a mistake
            // in tracking children.
            assert(roots.begin() == roots.end());
        }

        /*static*/ void link::mark_roots() {
            std::set<link*>::iterator nextIt;
            for (auto it = roots.begin(); it != roots.end(); it = nextIt) {
                nextIt = it;
                ++nextIt;
                link* l = *it;
                if (l->colour == colour_t::purple && l->ref_count > 0)
                    l->mark_grey();
                else {
                    l->buffered = false;
                    roots.erase(it);
                    if (l->colour == colour_t::black && l->ref_count == 0)
                        delete l;
                }
            }
        }

        /*static*/ void link::scan_roots() {
            std::set<link*>::iterator nextIt;
            for (auto it = roots.begin(); it != roots.end(); it = nextIt) {
                nextIt = it;
                ++nextIt;
                link* l = *it;
                l->scan();
            }
        }

        /*static*/ void link::collect_roots() {
            for (auto it = roots.begin(); it != roots.end(); ++it) {
                link* l = *it;
                l->buffered = false;
                l->collect_white();
            }
            link::roots.clear();
        }

        void link::mark_grey() {
            if (colour != colour_t::grey) {
                colour = colour_t::grey;
                for (auto it = children.begin(); it != children.end(); ++it) {
                    link* l = *it;
                    printf("dec %p\n", l);  // ###
                    fflush(stdout);  // ###
                    l->ref_count--;
                    l->mark_grey();
                }
            }
        }

        void link::scan() {
            if (colour == colour_t::grey) {
                if (ref_count > 0)
                    scan_black();
                else {
                    colour = colour_t::white;
                    for (auto it = children.begin(); it != children.end(); ++it) {
                        link* l = *it;
                        l->scan();
                    }
                }
            }
        }

        void link::scan_black() {
            colour = colour_t::black;
            for (auto it = children.begin(); it != children.end(); ++it) {
                link* l = *it;
                l->ref_count++;
                if (l->colour != colour_t::black)
                    l->scan_black();
            }
        }

        void link::collect_white() {
            if (colour == colour_t::white && !buffered) {
                colour = colour_t::black;
                for (auto it = children.begin(); it != children.end(); ++it) {
                    link* l = *it;
                    l->collect_white();
                }
                to_delete.insert(this);
            }
        }

        void link::capturer_acquire() {
            if (capturer != NULL)
                capturer->push_back(this);
        }

        void link::capturer_release() {
            if (capturer != NULL) {
                for (auto it = capturer->begin(); it != capturer->end(); ++it)
                    if (*it == this) {
                        capturer->erase(it);
                        break;
                    }
            }
        }
    }

    void collect_cycles() {
        impl::link::lock.lock();
        impl::link::collect_cycles();
        impl::link::lock.unlock();
    }
}

