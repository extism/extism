.PHONY: example
example:
	$(CXX) -std=c++11 -o example -I. example.cpp -lextism -ljsoncpp -L.

build-test:
	$(CXX) -std=c++11 -o test/test -I. test/test.cpp -lextism -lgtest -ljsoncpp -lpthread -lm -L.

.PHONY: test
test: build-test
	cd test && ./test
