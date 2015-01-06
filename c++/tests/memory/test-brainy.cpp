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

    virtual bool traverse_dependents(sodium::brainy::visitor& v)
    {
        return v.visit(next.untyped);
    }
};

int main(int argc, char* argv[])
{
    sodium::brainy_ptr<A> three(new A);
    {
        sodium::brainy_ptr<A> one(new A);
        sodium::brainy_ptr<A> two(new A);
        one->next = two;
        two->next = one;
        three->next = one;
        printf("deallocate one & two (three should keep them alive)\n");
    }
    printf("deallocate three\n");
}

