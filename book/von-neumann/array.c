#include <stdlib.h>
#include <assert.h>

typedef struct Node { unsigned value; } Node;

int main(int argc, char* argv[]) {
    const unsigned n = 1000000;
    const unsigned iterations = 1000;
    Node* nodes = malloc(sizeof(Node) * n);
    unsigned i, iter;
    Node* node;
    for (i = 0; i < n; i++)
        nodes[i].value = i;
    for (iter = 0; iter < iterations; iter++) {
        unsigned long long sum = 0;
        for (i = 0; i < n; i++)
            sum += nodes[i].value;
        assert(sum == (unsigned long long)(n - 1) * n / 2);
    }
}
