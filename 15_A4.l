%{
#ifndef NO_YY
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

{IDENT} { return IDENTIFIER;}
{INTCONST} { return INTCONST; }
{CHARCONST} { return CHARCONST; }
{STRLIT} { return STRING_LITERAL;}  

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