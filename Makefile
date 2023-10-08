CC=gcc
l=flex

o=lexer

team=recursive-dissent
assignment=2

build: clean $(o)

$(o): $(o).c
	$(CC) -o $(o) $^ $(team)_A$(assignment).c

$(o).c: $(team)_A$(assignment).l
	$(l) -o$(o).c $^

clean: 
	rm -rf $(o).*

test: $(o)
	./$(o) < $(team)_A$(assignment).nc

.PHONY: default clean test build