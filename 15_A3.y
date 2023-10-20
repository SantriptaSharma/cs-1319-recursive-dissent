%{
	#include <stdio.h> 
	#include <stdlib.h>

	extern int yylex();

	void yyerror(char *s);
%}

/* %token KEYWORD */
%token IDENTIFIER
%token CONSTANT
%token STRING_LITERAL
/* %token PUNCTUATOR */

%token ARROW
%token GEQ
%token LEQ
%token CEQ
%token NEQ
%token LAND
%token LOR

%start expression

%%
primary_expression:
	IDENTIFIER
	| CONSTANT
	| STRING_LITERAL
	| '(' expression ')'

postfix_expression:
	primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' argument_expression_list ')'
	| postfix_expression '(' ')'
	| postfix_expression ARROW IDENTIFIER

argument_expression_list:
	assignment_expression
	| argument_expression_list ',' assignment_expression

unary_expression:
	postfix_expression
	| unary_operator unary_expression

unary_operator:
	'&' | '*' | '+' | '-' | '!'

multiplicative_expression:
	unary_expression
	| multiplicative_expression '*' unary_expression
	| multiplicative_expression '/' unary_expression
	| multiplicative_expression '%' unary_expression

additive_expression:
	multiplicative_expression
	| additive_expression '+' multiplicative_expression
	| additive_expression '-' multiplicative_expression

relational_expression:
	additive_expression
	| relational_expression '<' additive_expression
	| relational_expression '>' additive_expression
	| relational_expression LEQ additive_expression
	| relational_expression GEQ additive_expression

equality_expression:
	relational_expression
	| equality_expression CEQ relational_expression
  	| equality_expression NEQ relational_expression

logical_AND_expression:
	equality_expression
	| logical_AND_expression LAND equality_expression

logical_OR_expression:
	logical_AND_expression
	| logical_OR_expression LOR logical_AND_expression

conditional_expression:
	logical_OR_expression
	| logical_OR_expression '?' expression ':' conditional_expression

assignment_expression:
	conditional_expression
	| unary_expression '=' assignment_expression

expression:
	assignment_expression
%%

void yyerror(char *s) {
	printf("%s\n", s);
}