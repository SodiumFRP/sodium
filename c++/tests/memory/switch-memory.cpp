#include <sodium/sodium.h>
#include <unistd.h>
#include <string>

using namespace sodium;
using namespace std;

/*!
 * Run:
 *     valgrind --tool=massif memory/switch-memory
 *     massif-visualizer massif.out.*
 *
 * What you should see:
 *
 *     flat memory usage profile (no leak)
 */
int main(int argc, char* argv[])
{
    #define N 100
    behavior<string>* as[N];
    behavior_sink<string>* bs[N];
    behavior_sink<behavior<string>>* ss[N];
    behavior<string>* os[N];
    std::function<void()> unlistens[N];
    {
        transaction t;
        for (int i = 0; i < N; i++) {
            as[i] = new behavior<string>("hello");
            bs[i] = new behavior_sink<string>("world");
            ss[i] = new behavior_sink<behavior<string>>(*as[i]);
            os[i] = new behavior<string>(switch_b(*ss[i]));
            os[i]->updates().listen([] (const string& s) {
                //printf("%s\n", s.c_str());
            });
        }
    }
    for (int iter = 0; iter < 100; iter++) {
        for (int i = 0; i < N; i++)
            ss[i]->send(*as[i]);
        for (int i = 0; i < N; i++)
            ss[i]->send(*bs[i]);
    }
    return 0;
}
