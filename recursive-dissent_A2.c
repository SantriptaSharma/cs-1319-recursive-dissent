#include <stdio.h>

int yylex();

int main() {
	while (yylex()) {}
	printf("\n");
}