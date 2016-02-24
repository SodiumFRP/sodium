#ifndef _CALM_H_
#define _CALM_H_

#include <sodium/sodium.h>

namespace calm_impl {
    /*!
     * Remove any sample from the stream that is equal to the previous one.
     */
    template <class A, class P = def_part>
    event<A> calm(const event<A>& input,
            const std::function<boost::optional<A>()>& oInit_lazy)
    {
        using namespace std;
        using namespace boost;

        return filter_optional<A>(
            input.template collect_lazy<optional<A>, optional<A>>(
                oInit_lazy,
                [] (const A& a, const optional<A>& lastA) -> tuple<optional<A>, optional<A>> {
                    optional<A> oa(a);
                    if (oa == lastA.get())  // Same as last: output nothing
                        return make_tuple(optional<A>(), lastA);
                    else
                        return make_tuple(oa, oa);
                }
            )
        );
    }
}

/*!
 * Remove any sample from the stream that is equal to the previous one.
 */
template <class A, class P = def_part>
event<A> calm(const event<A>& input)
{
    return calm_impl::calm<A>(input, [] () -> boost::optional<A> {
        return boost::optional<A>();
    });
}

/*!
 * Stop values propagating that are equal to the previous one.
 */
template <class A, class P = def_part>
behavior<A> calm(const behavior<A>& input)
{
    using namespace std;
    using namespace boost;

    transaction trans;
    function<A()> init = input.sample_lazy();
    return calm_impl::calm<A>(input.updates(),
        [init] () -> optional<A> {
            return optional<A>(init());
        }).hold_lazy(init);
}

#endif

