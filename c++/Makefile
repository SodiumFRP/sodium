all: test_sodium

CPPFLAGS=-I. --std=c++11
OBJECT_FILES=sodium/light_ptr.o sodium/sodium.o sodium/test_sodium.o sodium/transaction.o

test_sodium: $(OBJECT_FILES)
	echo $(CXX) -o $< $@

clean:
	rm -f $(OBJECT_FILES) test_sodium
