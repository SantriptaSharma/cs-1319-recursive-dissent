CC=gcc
l=flex
yy=bison
CFLAGS=-Werror -lfl -g

o=translator
lx=lexer

team=15
assignment=5

testfile=testthing.nc

fname=$(team)_A$(assignment)

build: $(o)

test: build
	./$(o) < $(testfile)

$(o): $(fname)_translator.o $(fname).tab.o $(lx).o compiler.o
	$(CC) -o $@ $^ $(CFLAGS)

$(lx): lexmain.o $(lx).o
	$(CC) -o $@ $^

$(fname).tab.c: $(fname).y
	$(yy) -d $^ -t

$(lx).c: $(fname).l $(fname).tab.c
	$(l) -o$@ $<

%.o: %.c
	$(CC) -c $^ $(CFLAGS)

clean: 
	rm -rf $(o)*
	rm -rf $(lx)*
	rm -rf *.o
	rm -rf $(fname).tab.*

.PHONY: default clean build lex