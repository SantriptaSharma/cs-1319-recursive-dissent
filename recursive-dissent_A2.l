%{
// #include <stdlib.h>
// #include <stdio.h>
// #include <string.h>flex 

#define KEYWORD 10
#define IDENT 11
#define CONST 12
#define STRLIT 13
#define PUNCT 14

// typedef struct {
// 	int id;
// 	const char *lexeme;
// } Token;

// struct {
// 	int capacity;
// 	int size;
// 	Token *tokens;
// } token_stream;

// void init_stream() {
// 	token_stream.capacity = 8;
// 	token_stream.size = 0;
// 	token_stream.tokens = calloc(token_stream.capacity, sizeof(*token_stream.tokens));
// }

// int push(int id, const char * in_stream_lexeme) {
// 	Token t = { .id = id, .lexeme = strdup(in_stream_lexeme) };
	
// 	int ind = token_stream.size;
// 	token_stream.tokens[token_stream.size++] = t;

// 	if (token_stream.size >= token_stream.capacity) {
// 		token_stream.capacity *= 2;
// 		token_stream.tokens = realloc(token_stream.tokens, token_stream.capacity);
// 	}

// 	return ind; sd
// }

// void free_stream() {
// 	for (int i = 0; i < token_stream.size; i++) {
// 		free((void *)token_stream.tokens[i].lexeme);
// 	}

// 	free((void *)token_stream.tokens);
// }

// const char * const token_names[] = {
// 	[KEYWORD] = "KEYWORD",
// 	[IDENT] = "IDENTIFIER",
// 	[CONST] = "CONSTANT",
// 	[STRLIT] = "STRING LITERAL",
// 	[PUNCT] = "PUNCTUATION"
// };

// void display(const Token t) {
// 	printf("<%s, %s>, ", token_names[t.id], t.lexeme);
// }
// [S|^S]  // 
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
COMMENTMULTI \/\*(.|[\s\n])*\*\/


%%
{KEYWORD} {printf("<KEYWORD, %s>, ", yytext); return 1; }
{IDENT} {printf("<IDENT, %s>," , yytext); return 1;}
{PUNCT} {printf("<PUNCT, %s>, ", yytext); return 1; }
{CONST} {printf("<CONST, %s>, ", yytext); return 1; }
{STRLIT} {printf("<STRLIT, %s>, ", yytext); return 1;}
{COMMENTSINGLE}|{COMMENTMULTI} {return 1;}

[\s\n\t]* { return 1; }

%%

int yywrap() { return 1; }