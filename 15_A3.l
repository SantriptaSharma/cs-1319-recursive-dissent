%{
#ifndef NO_YY
	#include "15_A3.tab.h"
#endif
%}

KEYWORD char|else|for|if|int|return|void
IDENT [a-zA-Z\_][0-9a-zA-Z\_]*
PUNCT \[|\]|\(|\)|\{|\}|->|&|\*|\+|-|\/|%|!|\?|\<|\>|<=|>=|==|!=|&&|\\|\||=|:|;|,
ESCAPE \\'|\\\?|\\\"|\\\\|\\a|\\b|\\f|\\n|\\r|\\t|\\v
CHAR [^\\'\n]|{ESCAPE}
CONST ([\+-]?[1-9][0-9]*)|[0-9]+|'{CHAR}+'
STRCHAR [^\"\\\n]|{ESCAPE}
STRLIT \"{STRCHAR}*\"
COMMENTSINGLE \/\/([^\n])*\n
COMMENTMULTI \/\*([^\*]|\*[^\/])*\*\/



%%
{IDENT} { return IDENTIFIER;}
{CONST} { return CONSTANT; }
{STRLIT} { return STRING_LITERAL;}  

"->" { return ARROW; }
">=" { return GEQ; }
"<=" { return LEQ; }
"==" { return CEQ; }
"!=" { return NEQ; }
"&&" { return LAND; }
"||" { return LOR; }

{COMMENTSINGLE}|{COMMENTMULTI} {}

[ \n\t]* {}

. { return yytext[0]; }
%%

int yywrap() { return 1; }