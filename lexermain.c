#include <stdio.h>

int yylex();

int yydebug;

int main(int argc, const char *argv[]) {
	int t;
	while (t = yylex()) {
		printf(t < 256 && t >= 0 ? "%c, " : "%d, ", t);
	}
}