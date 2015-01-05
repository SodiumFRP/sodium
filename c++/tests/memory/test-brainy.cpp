#include <sodium/brainy_ptr.h>

struct A : public sodium::brainy::dependents
{
    A() {
        printf("%p new\n", this);
    }
    virtual ~A()
    {
        printf("%p delete\n", this);
    }

    sodium::brainy_ptr<A> next;

    virtual void traverse_dependents(sodium::brainy::visitor& v)
    {
        v.visit(next.untyped);
    }
};

int main(int argc, char* argv[])
{
    sodium::brainy_ptr<A> one(new A);
    sodium::brainy_ptr<A> two(new A);
    one->next = two;
    two->next = one;
}

