CC=gcc
l=flex

o=lexer

team=recursive-dissent
assignment=2

build: clean $(o).exe

$(o).exe: $(o).c
	$(CC) -o $(o).exe $^ $(team)_A$(assignment).c

$(o).c: $(team)_A$(assignment).l
	$(l) -o$(o).c $^

clean: 
	rm -rf $(o).*

test: $(o).exe
	./$(o).exe < $(team)_A$(assignment).nc

.PHONY: default clean test build