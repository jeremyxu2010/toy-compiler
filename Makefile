all: compile

OBJS = 	parser.o  \
		codegen.o \
		main.o    \
		tokens.o  \
		corefn.o  \
		native.o  \

LLVMCONFIG = llvm-config
CPPFLAGS = `$(LLVMCONFIG) --cppflags` -std=c++14
LDFLAGS = `$(LLVMCONFIG) --ldflags` -lz -lncurses
LIBS = `$(LLVMCONFIG) --libs`
CC = clang
CXX = clang++

clean:
	$(RM) -rf parser.cpp parser.hpp compiler tokens.cpp $(OBJS) test/example.bc test/example.S test/example.native

parser.cpp: parser.y
	bison -d -o $@ $^

parser.hpp: parser.cpp

tokens.cpp: tokens.l parser.hpp
	flex -o $@ $^

%.o: %.cpp
	$(CXX) -c $(CPPFLAGS) -o $@ $<


compiler: $(OBJS)
	$(CXX) -o $@ $(OBJS) $(LIBS) $(LDFLAGS)

compile: compiler test/example.txt
	./compiler
	sleep 1
	llc test/example.bc -o test/example.S
	sleep 1
	$(CXX) native.cpp test/example.S -o test/example.native
