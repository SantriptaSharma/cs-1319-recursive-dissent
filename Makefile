CC=gcc
l=flex
yy=bison
CFLAGS=-Werror -lfl


o=parser
lx=lexer

team=15
assignment=3

fname=$(team)_A$(assignment)

build: clean $(o)
lex: clean $(lx)

$(o): $(team)_A$(assignment).tab.c $(lx).c
	$(CC) -o $(o) $^ $(fname).c $(CFLAGS)

$(lx): $(lx).c $(fname).tab.c
	$(CC) -o $(lx) $< lexermain.c

$(fname).tab.c: $(fname).y
	$(yy) -d $^ -t

$(lx).c: $(fname).l
	$(l) -o$(lx).c $^

clean: 
	rm -rf $(o)*
	rm -rf $(lx)*
	rm -rf $(fname).tab.*

.PHONY: default clean build lex