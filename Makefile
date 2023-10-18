CC=gcc
l=flex
CFLAGS=-Wall -Wextra -Werror -pedantic -std=c11 -lfl

o=parser
lx=lexer

team=15
assignment=3

build: clean $(o)

$(o): $(lx).c
	$(CC) $(CFLAGS) -o $(o) $^ $(team)_A$(assignment).c

$(lx).c: $(team)_A$(assignment).l
	$(l) -o$(lx).c $^

clean: 
	rm -rf $(o).*
	rm -rf $(lx).*

test: $(o)
	./$(o) < $(team)_A$(assignment).nc

.PHONY: default clean test build