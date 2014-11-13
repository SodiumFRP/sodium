#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct Node {
    unsigned value;
} Node;

void shuffle(Node* nodes, unsigned n)
{
    unsigned i;
    for (i = 0; i < n; i++) {
        unsigned j = (unsigned)(((long long)random() * n) / ((long long)RAND_MAX + 1));
        if (i != j) {
            Node node = nodes[i];
            nodes[i] = nodes[j];
            nodes[j] = node;
        }
    }
}

int main(int argc, char* argv[])
{
    const unsigned n = 1000000;
    const unsigned iterations = 1000;
    Node* nodes = malloc(sizeof(Node) * n);
    unsigned i;
    for (i = 0; i < n; i++)
        nodes[i].value = i;
    shuffle(nodes, n);
    {
        Node* node;
        unsigned iter;
        for (iter = 0; iter < iterations; iter++) {
            unsigned long long sum = 0;
            for (i = 0; i < n; i++)
                sum += nodes[i].value;
            assert(sum == (unsigned long long)(n - 1) * n / 2);
        }
    }
}
