all: test_sodium

CPPFLAGS=-I. --std=c++11 -g
OBJECT_FILES=sodium/light_ptr.o sodium/sodium.o sodium/test_sodium.o sodium/transaction.o
sodium/light_ptr.o: sodium/light_ptr.h
sodium/sodium.o: sodium/sodium.h sodium/transaction.h sodium/light_ptr.h
sodium/test_sodium.o: sodium/test_sodium.h sodium/sodium.h sodium/transaction.h sodium/light_ptr.h
sodium/transaction.o: sodium/transaction.h

test_sodium: $(OBJECT_FILES)
	$(CXX) -o $@ $(OBJECT_FILES) -lpthread -lcppunit

clean:
	rm -f $(OBJECT_FILES) test_sodium
