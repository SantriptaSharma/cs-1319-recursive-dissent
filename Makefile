CC=gcc
l=flex

o=parser

team=recursive-dissent
assignment=2

default: clean $(o).exe

$(o).exe: $(o).c
	$(CC) -o $(o).exe $^ $(team)_A$(assignment).c

$(o).c: $(team)_A$(assignment).l
	$(l) -o$(o).c $^

clean: 
	rm -rf $(o).*

test: $(o).exe
	./$(o).exe < $(team)_A$(assignment).nc

.PHONY: default clean test