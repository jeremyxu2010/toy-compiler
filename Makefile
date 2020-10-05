all: compile

OBJS = parser.o  \
       codegen.o \
       main.o    \
       tokens.o  \
       corefn.o  \
	   native.o  \

LLVMCONFIG = llvm-config
CPPFLAGS = `$(LLVMCONFIG) --cppflags` -std=c++14
LDFLAGS = `$(LLVMCONFIG) --ldflags` -lpthread -ldl -lz -lncurses -rdynamic
LIBS = `$(LLVMCONFIG) --libs`

clean:
	$(RM) -rf parser.cpp parser.hpp compiler tokens.cpp $(OBJS) test/example.bc test/example.S test/example.native

parser.cpp: parser.y
	bison -d -o $@ $^
	
parser.hpp: parser.cpp

tokens.cpp: tokens.l parser.hpp
	flex -o $@ $^

%.o: %.cpp
	g++ -c $(CPPFLAGS) -o $@ $<


compiler: $(OBJS)
	g++ -o $@ $(OBJS) $(LIBS) $(LDFLAGS)

compile: compiler test/example.txt
	./compiler
	llc test/example.bc -o test/example.S
	gcc native.cpp test/example.S -o test/example.native
