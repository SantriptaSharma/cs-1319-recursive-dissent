#include <stdio.h>

extern int yylex();

int main() {
	int c;

	while (c = yylex()) {
		if (c < 256) {
			printf("%c, ", c);
		} else {
			printf("%d, ", c);
		}
	}
}