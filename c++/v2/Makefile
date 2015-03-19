CXXFLAGS=--std=c++11 -g
CPPFLAGS=-I. -DSODIUM_OLD_NAME_COMPATIBILITY
LDFLAGS=-lpthread
OBJS=sodium/impl/magic_ref.o \
     sodium/impl/mutex.o \
     sodium/transaction.o

all: test_sodium test

clean:
	rm -f $(OBJS) test_sodium.o test.o test_sodium test

sodium/impl/mutex.h: sodium/impl/common.h
sodium/impl/magic_ref.h: sodium/impl/mutex.h
sodium/impl/magic_ref.cpp: sodium/impl/magic_ref.h sodium/impl/mutex.h
sodium/transaction.cpp: sodium/transaction.h sodium/impl/magic_ref.h sodium/impl/mutex.h
sodium/sodium.h: sodium/transaction.h sodium/impl/magic_ref.h sodium/impl/mutex.h
test.o: sodium/impl/magic_ref.h sodium/impl/mutex.h

test_sodium.o: ../tests/test_sodium.cpp ../tests/test_sodium.h \
        sodium/sodium.h sodium/transaction.h sodium/impl/magic_ref.h \
        sodium/impl/mutex.h sodium/impl/common.h
	g++ $(CPPFLAGS) $(CXXFLAGS) -g -o test_sodium.o -c $<

test: test.o $(OBJS)
	g++ -g -o test test.o $(OBJS) $(LDFLAGS)

test_sodium: test_sodium.o $(OBJS)
	g++ -g -o test_sodium test_sodium.o $(OBJS) $(LDFLAGS) -lcppunit

