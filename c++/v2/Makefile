CPPFLAGS=-I. --std=c++11 -g
LDFLAGS=-lpthread
OBJS=test.o sodium/impl/magic_ref.o sodium/impl/mutex.o

all: test

clean:
	rm -f $(OBJS)

sodium/impl/magic_ref.h: sodium/impl/mutex.h
test.o: sodium/impl/magic_ref.h sodium/impl/mutex.h

test: $(OBJS)
	g++ -g -o test $(OBJS) $(LDFLAGS)
