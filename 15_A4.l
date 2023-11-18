%{
#include <string.h>
#include <stdlib.h>
#ifndef NO_YY
	#include "15_A4_translator.h"
	#include "15_A4.tab.h"
#endif
%}

IDENT [a-zA-Z\_][0-9a-zA-Z\_]*
ESCAPE \\'|\\\?|\\\"|\\\\|\\a|\\b|\\f|\\n|\\r|\\t|\\v
CHAR [^\\'\n]|{ESCAPE}
CHARCONST \'{CHAR}+\'
INTCONST [0-9]*
STRCHAR [^\"\\\n]|{ESCAPE}
STRLIT \"{STRCHAR}*\"
COMMENTSINGLE \/\/([^\n])*\n
COMMENTMULTI \/\*([^\*]|\*[^\/])*\*\/



%%
"void" { return VOID; }
"char" { return CHAR; }
"int" { return INT; }
"if" { return IF; }
"else" { return ELSE; }
"for" { return FOR; }
"return" { return RETURN; }

{IDENT} { yylval.string = yytext; return IDENTIFIER;}
{INTCONST} { yylval.val = atoi(yytext); return INTCONST; }

	/* TODO: Account for multibyte character constants (shift and accumulate) */
{CHARCONST} { 
	yylval.val = (int) yytext[1]; return CHARCONST; 
}
{STRLIT} {
	size_t len = strlen(yytext);
	char *val = malloc(len - 1);

	memcpy(val, yytext + 1, len - 2);
	val[len - 2] = '\0';

	yylval.string = val; return STRING_LITERAL;
}  

"->" { return ARROW; }
">=" { return GEQ; }
"<=" { return LEQ; }
"==" { return CEQ; }
"!=" { return NEQ; }
"&&" { return LAND; }
"||" { return LOR; }

{COMMENTSINGLE}|{COMMENTMULTI} {}

[ \n\t\r]* {}

. { return yytext[0]; }
%%

int yywrap() { return 1; }