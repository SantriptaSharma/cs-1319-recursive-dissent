#include <stdio.h>
#include <stdlib.h>

int yyparse();
int yylex();

extern int yydebug;

int main(int argc, const char *argv[]) {
	while (argc > 1) {
		const char *f = argv[--argc];

		if (f[0] != '-') exit(1);

		switch(f[1]) {
			case 'd':
				yydebug = 1;
			break;
		}
	}

	yyparse();
}