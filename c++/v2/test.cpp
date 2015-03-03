#include <sodium/impl/magic_ref.h>
#include <functional>

template <class A>
void dump(const char* descr, const sodium::impl::magic_ref<A>& ra)
{
    printf("%s: %p ->", descr, ra.l());
    for (auto it = ra.children().begin(); it != ra.children().end(); ++it)
        printf(" %p", *it);
    printf("\n");
}

int main(int argc, char* argv[])
{
    using namespace sodium::impl;
    using namespace std;

    /*
    magic_ref<int> a;
    a.assign(5);
    magic_ref<std::function<void()>> b;
    b.assign([a] () {
        dump("a", a);
    });
    dump("b", b);
    magic_ref<magic_ref<int>> c;
    c.assign(a);
    dump("c", c);
    */

    {
        magic_ref<std::function<void()>> d;
        magic_ref<magic_ref<std::function<void()>>> e;
        magic_ref<std::function<void()>> e0;
        e.assign(e0);
        d.assign([e] () {
            dump("e", e);
        });
        e->assign([d] () {
            dump("d", d);
        });
        dump("d", d);
        dump("e", e);
        dump("*e", *e);
    }
    sodium::collect_cycles();
}
