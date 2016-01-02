#include <sodium/sodium.h>
#include <unistd.h>
#include <string>

using namespace sodium;
using namespace std;

/*!
 * Run:
 *     valgrind --tool=massif --time-unit=ms memory/release-sink-machinery
 *     massif-visualizer massif.out.*
 *
 * What you should see:
 *
 *   +--+
 *   |  |
 *   |  |
 *   |  |
 *   |  |
 *   |  |
 *   |  |
 *   |  |
 *   |  +--+
 *   |     |
 *   +     +---+
 */
int main(int argc, char* argv[])
{
    #define N 1000
    behavior_sink<behavior<string>>* ss[N];
    behavior<string>* s[N];
    {
        printf("constructing\n");
        transaction t;
        for (int i = 0; i < N; i++) {
            ss[i] = new behavior_sink<behavior<string>>(behavior<string>(string()));
            s[i] = new behavior<string>(switch_b(*ss[i]));
        }
        printf("done\n");
    }
    for (int i = 0; i < 200; i++) {
        free(malloc(5));
        usleep(10000);
    }
    printf("deleting 1\n");
    for (int i = 0; i < N; i++)
        delete ss[i];
    printf("done\n");
    for (int i = 0; i < 200; i++) {
        free(malloc(5));
        usleep(10000);
    }
    printf("deleting 2\n");
    for (int i = 0; i < N; i++)
        delete s[i];
    printf("done\n");
    for (int i = 0; i < 200; i++) {
        free(malloc(5));
        usleep(10000);
    }
    return 0;
}
