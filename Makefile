CC=gcc
l=flex
yy=bison
CFLAGS=-Werror -lfl -g
SFLAGS=-no-pie -g

o=compiler
lx=lexer

team=15
assignment=5

testfile=testthing.nc

fname=$(team)_A$(assignment)

build: $(o)

# TODO: write this according to spec
# test: build
# 	./$(o) < $(testfile)

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

ltest: build
	./$(o) test < compilertest.nc
	gcc -c test.s $(SFLAGS)
	gcc -o testexec test.o $(SFLAGS)

clean: 
	rm -rf $(o)
	rm -rf $(o).exe
	rm -rf $(lx)*
	rm -rf *.o
	rm -rf $(fname).tab.*

.PHONY: default clean build lex test