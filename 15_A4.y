%{
	#include <stdio.h> 
	#include <stdlib.h>

	#include "15_A4_translator.h"

	#define log(...) printf(__VA_ARGS__); printf("\n");

	extern int yylex();
	extern const char *yytext;

	void yyerror(char *s);
%}

%union {
	ExprAttrib expr;
	const char *string;
	int val;	
}

/* %token KEYWORD */

%token <expr> IDENTIFIER
%token <val> INTCONST
%token <val> CHARCONST
// TODO: figure out how to safely alloc/dealloc this (store a pointer to a const strings table?)
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

%type <val> constant
%type <expr> primary_expression
%type <expr> postfix_expression
%type <expr> unary_expression
%type <expr> multiplicative_expression
%type <expr> additive_expression
%type <expr> relational_expression
%type <expr> equality_expression
%type <expr> logical_AND_expression
%type <expr> logical_OR_expression
%type <expr> conditional_expression
%type <expr> assignment_expression
%type <expr> expression 

%start translation_unit

%%
/* expressions */
constant:
	INTCONST
	| CHARCONST

primary_expression:
	IDENTIFIER
	| constant {$$ = PURE_EXPR(GenTemp()); Emit(Mov(ASym($$), AImm($1)));}
	| STRING_LITERAL {
		// TODO: figure out string storage
		log("primary-expression")
	}
	| '(' expression ')' {$$ = $2;}

postfix_expression:
	primary_expression
	| postfix_expression '[' expression ']' {
		// TODO: add error detection, make sure $1 is a pointer
		$$ = PURE_EXPR(GenTemp()); Emit(IndexRead(ASym($$), ASym($1), ASym($3)));
	}
	| postfix_expression '(' argument_expression_list ')' {
		// TODO: call, validate number and type of params
		log("postfix-expression")
	}
	| postfix_expression '(' ')' {
		// TODO: call, validate number and type of params
		log("postfix-expression")
	}
	| postfix_expression ARROW IDENTIFIER {
		// TODO: not sure what to do here? we don't have structs
		log("postfix-expression")
	}

// TODO: build assignment expression lists
argument_expression_list:
	assignment_expression {log("argument-expression-list")}
	| argument_expression_list ',' assignment_expression {log("argument-expression-list")}

// TODO: write out relational actions using dummy keys

unary_expression:
	postfix_expression
	| '&' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(UnaryOp(ADDR, ASym($$), ASym($2)));}
	| '*' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(UnaryOp(DEREF, ASym($$), ASym($2)));}
	| '+' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(UnaryOp(POS, ASym($$), ASym($2)));}
	| '-' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(UnaryOp(NEG, ASym($$), ASym($2)));}
	| '!' unary_expression {$$ = $2}

multiplicative_expression:
	unary_expression
	| multiplicative_expression '*' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(BinOp(MUL, ASym($$), ASym($1), ASym($3)));}
	| multiplicative_expression '/' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(BinOp(DIV, ASym($$), ASym($1), ASym($3)));}
	| multiplicative_expression '%' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(BinOp(MOD, ASym($$), ASym($1), ASym($3)));}

additive_expression:
	multiplicative_expression
	| additive_expression '+' multiplicative_expression {$$ = PURE_EXPR(GenTemp()); Emit(BinOp(ADD, ASym($$), ASym($1), ASym($3)));}
	| additive_expression '-' multiplicative_expression {$$ = PURE_EXPR(GenTemp()); Emit(BinOp(SUB, ASym($$), ASym($1), ASym($3)));}

relational_expression:
	additive_expression
	| relational_expression '<' additive_expression {log("relational-expression")}
	| relational_expression '>' additive_expression {log("relational-expression")}
	| relational_expression LEQ additive_expression {log("relational-expression")}
	| relational_expression GEQ additive_expression {log("relational-expression")}

equality_expression:
	relational_expression
	| equality_expression CEQ relational_expression {log("equality-expression")}
  	| equality_expression NEQ relational_expression {log("equality-expression")}

logical_AND_expression:
	equality_expression
	| logical_AND_expression LAND equality_expression {log("logical-AND-expression")}

logical_OR_expression:
	logical_AND_expression
	| logical_OR_expression LOR logical_AND_expression {log("logical-OR-expression")}

conditional_expression:
	logical_OR_expression
	| logical_OR_expression '?' expression ':' conditional_expression {log("conditional-expression")}

assignment_expression:
	conditional_expression
	| unary_expression '=' assignment_expression {log("assignment-expression")}

expression:
	assignment_expression

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
	external_declaration {log("translation-unit")}
	| translation_unit external_declaration {log("translation-unit")}

external_declaration:
	function_definition {log("external-declaration")}
	| declaration {log("external-declaration")}
	
function_definition:
	type_specifier declarator compound_statement {log("function-definition")}
%%

void yyerror(char *s) {
	printf("Error: %s on '%s'\n", s, yytext);
}