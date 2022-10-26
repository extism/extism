FLAGS=`pkg-config --cflags --libs jsoncpp gtest` -lextism -lpthread

build-example:
	$(CXX) -std=c++11 -o example -I. example.cpp $(FLAGS)
	
.PHONY: example
example: build-example
	./example

build-test:
	$(CXX) -std=c++11 -o test/test -I. test/test.cpp $(FLAGS)

.PHONY: test
test: build-test
	cd test && ./test
