%{

#include "15_A3.tab.h"

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
{KEYWORD} { return KEYWORD; }
{IDENT} { return IDENTIFIER;}
{PUNCT} { return PUNCTUATOR; }
{CONST} { return CONSTANT; }
{STRLIT} { return STRING_LITERAL;}  

{COMMENTSINGLE}|{COMMENTMULTI} {return 1;}

[ \n\t]* {return 1;}

. { return 0; }

%%

int yywrap() { return 1; }