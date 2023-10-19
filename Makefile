CC=gcc
l=flex
CFLAGS=-Werror -lfl

o=parser
lx=lexer

team=15
assignment=3

build: clean $(o)

$(o): $(team)_A$(assignment).tab.c $(lx).c
	$(CC) -o $(o) $^ $(team)_A$(assignment).c $(CFLAGS)

$(team)_A$(assignment).tab.c: $(team)_A$(assignment).y
	bison -d $^

$(lx).c: $(team)_A$(assignment).l
	$(l) -o$(lx).c $^

clean: 
	rm -rf $(o).*
	rm -rf $(lx).*
	rm -rf $(team)_A$(assignment).tab.*

test: $(o)
	./$(o) < $(team)_A$(assignment).nc

.PHONY: default clean test build