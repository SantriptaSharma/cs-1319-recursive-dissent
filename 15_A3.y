%{
	#include <stdio.h> 
	#include <stdlib.h>

	extern int yylex();

	void yyerror(char *s);
%}

/* %token KEYWORD */

%token IDENTIFIER
%token INTCONST
%token CHARCONST
%token STRING_LITERAL

/* %token PUNCTUATOR */
%token ARROW
%token GEQ
%token LEQ
%token CEQ
%token NEQ
%token LAND
%token LOR
%token VOID
%token CHAR
%token INT
%token IF
%token ELSE
%token FOR
%token RETURN

%start translation_unit

%%
/* expressions */
constant:
	INTCONST
	| CHARCONST

primary_expression:
	IDENTIFIER
	| constant
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

/* Declarations */

declaration:
	type_specifier init_declarator

init_declarator:
	declarator
	| declarator '=' initalizer
	
type_specifier:
	VOID
	| CHAR
	| INT

declarator:
	pointer direct_declarator
	| direct_declarator

direct_declarator:
	IDENTIFIER
	| IDENTIFIER '[' INTCONST ']'
	| IDENTIFIER '(' parameter_list ')'
	| IDENTIFIER '(' ')'

pointer:
	'*'

parameter_list:
	parameter_declaration
	| parameter_list ',' parameter_declaration
	
parameter_declaration:
	type_specifier pointer IDENTIFIER
	| type_specifier IDENTIFIER
	| type_specifier pointer
	| type_specifier

initalizer:
	assignment_expression

/* Statements */
statement:
	compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement

compound_statement:
	'{' '}'
	| '{' block_item_list '}'
	
block_item_list:
	block_item
	| block_item_list block_item

block_item:
	declaration
	| statement

opt_expression:
	expression
	| /* empty */

expression_statement:
	opt_expression ';'

selection_statement:
	IF '(' expression ')' statement
	| IF '(' expression ')' statement ELSE statement

iteration_statement:
	FOR '(' opt_expression ';' opt_expression ';' opt_expression ')' statement
	
jump_statement:
	RETURN opt_expression ';'

/* TLU */
translation_unit:
	function_definition
	| declaration
	
function_definition:
	type_specifier declarator compound_statement
%%

void yyerror(char *s) {
	printf("%s\n", s);
}