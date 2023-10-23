%{
	#include <stdio.h> 
	#include <stdlib.h>

	#define log(...) printf(__VA_ARGS__); printf("\n");

	extern int yylex();
	extern const char *yytext;

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
	IDENTIFIER {log("primary-expression")}
	| constant {log("primary-expression")}
	| STRING_LITERAL {log("primary-expression")}
	| '(' expression ')' {log("primary-expression")}

postfix_expression:
	primary_expression {log("postfix-expression")}
	| postfix_expression '[' expression ']' {log("postfix-expression")}
	| postfix_expression '(' argument_expression_list ')' {log("postfix-expression")}
	| postfix_expression '(' ')' {log("postfix-expression")}
	| postfix_expression ARROW IDENTIFIER {log("postfix-expression")}

argument_expression_list:
	assignment_expression {log("argument-expression-list")}
	| argument_expression_list ',' assignment_expression {log("argument-expression-list")}

unary_expression:
	postfix_expression {log("unary-expression")}
	| unary_operator unary_expression {log("unary-expression")}

unary_operator:
	'&' {log("unary-operator")} | '*' {log("unary-operator")} | '+' {log("unary-operator")} | '-' {log("unary-operator")} | '!' {log("unary-operator")} 

multiplicative_expression:
	unary_expression {log("multiplicative-expression")}
	| multiplicative_expression '*' unary_expression {log("multiplicative-expression")}
	| multiplicative_expression '/' unary_expression {log("multiplicative-expression")}
	| multiplicative_expression '%' unary_expression {log("multiplicative-expression")}

additive_expression:
	multiplicative_expression {log("additive-expression")}
	| additive_expression '+' multiplicative_expression {log("additive-expression")}
	| additive_expression '-' multiplicative_expression {log("additive-expression")}

relational_expression:
	additive_expression {log("relational-expression")}
	| relational_expression '<' additive_expression {log("relational-expression")}
	| relational_expression '>' additive_expression {log("relational-expression")}
	| relational_expression LEQ additive_expression {log("relational-expression")}
	| relational_expression GEQ additive_expression {log("relational-expression")}

equality_expression:
	relational_expression {log("equality-expression")}
	| equality_expression CEQ relational_expression {log("equality-expression")}
  	| equality_expression NEQ relational_expression {log("equality-expression")}

logical_AND_expression:
	equality_expression {log("logical-AND-expression")}
	| logical_AND_expression LAND equality_expression {log("logical-AND-expression")}

logical_OR_expression:
	logical_AND_expression {log("logical-OR-expression")}
	| logical_OR_expression LOR logical_AND_expression {log("logical-OR-expression")}

conditional_expression:
	logical_OR_expression {log("conditional-expression")}
	| logical_OR_expression '?' expression ':' conditional_expression {log("conditional-expression")}

assignment_expression:
	conditional_expression {log("assignment-expression")}
	| unary_expression '=' assignment_expression {log("assignment-expression")}

expression:
	assignment_expression {log("expression")}

/* Declarations */

declaration:
	type_specifier init_declarator ';' {log("declaration")}

init_declarator:
	declarator {log("init-declarator")}
	| declarator '=' initalizer {log("init-declarator")}
	
type_specifier:
	VOID {log("type-specifier")}
	| CHAR {log("type-specifier")}
	| INT {log("type-specifier")}

declarator:
	pointer direct_declarator {log("declarator")}
	| direct_declarator {log("declarator")}

direct_declarator:
	IDENTIFIER {log("direct-declarator")}
	| IDENTIFIER '[' INTCONST ']' {log("direct-declarator")}
	| IDENTIFIER '(' parameter_list ')' {log("direct-declarator")}
	| IDENTIFIER '(' ')' {log("direct-declarator")}

pointer:
	'*' {log("pointer")}

parameter_list:
	parameter_declaration {log("parameter-list")}
	| parameter_list ',' parameter_declaration {log("parameter-list")}
	
parameter_declaration:
	type_specifier pointer IDENTIFIER {log("parameter-declaration")}
	| type_specifier IDENTIFIER {log("parameter-declaration")}
	| type_specifier pointer {log("parameter-declaration")}
	| type_specifier {log("parameter-declaration")}

initalizer:
	assignment_expression {log("initializer")}

/* Statements */
statement:
	compound_statement {log("statement")}
	| expression_statement {log("statement")}
	| selection_statement {log("statement")}
	| iteration_statement {log("statement")}
	| jump_statement {log("statement")}

compound_statement:
	'{' '}' {log("compound-statement")}
	| '{' block_item_list '}' {log("compound-statement")}
	
block_item_list:
	block_item {log("block-item-list")}
	| block_item_list block_item {log("block-item-list")}

block_item:
	declaration {log("block-item")}
	| statement {log("block-item")}

opt_expression:
	expression 
	| /* empty */

expression_statement:
	opt_expression ';' {log("expression-statement")}

selection_statement:
	IF '(' expression ')' statement {log("selection-statement")}
	| IF '(' expression ')' statement ELSE statement {log("selection-statement")}

iteration_statement:
	FOR '(' opt_expression ';' opt_expression ';' opt_expression ')' statement {log("iteration-statement")}
	
jump_statement:
	RETURN opt_expression ';' {log("jump-statement")}

/* TLU */
translation_unit:
	function_definition {log("translation-unit")}
	| declaration {log("translation-unit")}
	
function_definition:
	type_specifier declarator compound_statement {log("function-definition")}
%%

void yyerror(char *s) {
	printf("Error: %s on '%s'\n", s, yytext);
}