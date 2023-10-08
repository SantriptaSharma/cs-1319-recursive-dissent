%{

#define KEYWORD 10
#define IDENT 11
#define CONST 12
#define STRLIT 13
#define PUNCT 14

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
{KEYWORD} {printf("<KEYWORD, %s>, ", yytext); return 1; }
{IDENT} {printf("<IDENT, %s>, " , yytext); return 1;}
{PUNCT} {printf("<PUNCT, %s>, ", yytext); return 1; }
{CONST} {printf("<CONST, %s>, ", yytext); return 1; }
{STRLIT} {printf("<STRLIT, %s>, ", yytext); return 1;}  

{COMMENTSINGLE}|{COMMENTMULTI} {return 1;}

[ \n\t]* {return 1;}

. { return 0; }

%%

int yywrap() { return 1; }