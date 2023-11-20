%{
	#include <stdio.h> 
	#include <stdlib.h>
	#include <string.h>

	#include "15_A4_translator.h"

	#define log(...) printf(__VA_ARGS__); printf("\n");

	extern int yylex();
	extern const char *yytext;

	void yyerror(char *s);
%}

/* 
TODO: go through all rules, make sure no rules just logging remain 
TODO: write everything up in assignment pdf   
*/


%union {
	ExprAttrib expr;
	Symbol *decl;
	ArgList *argument_list;
	ArgListElem arg;
	char *string;
	int val;

	PRIMITIVE_TYPE type_spec;
}

/* %token KEYWORD */

%token <string> IDENTIFIER
%token <val> INTCONST
%token <val> CHARCONST
%token <string> STRING_LITERAL

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
%type <argument_list> argument_expression_list

%type <type_spec> type_specifier
%type <decl> declaration
%type <decl> init_declarator
%type <decl> declarator
%type <decl> direct_declarator
%type <argument_list> parameter_list
%type <arg> parameter_declaration
%type <expr> initializer

%start translation_unit

%%
/* expressions */
/* TODO: validate types and add implicit conversions 
	Compatible Types: (int, char, temp, array, any_ptr)
*/
constant:
	INTCONST
	| CHARCONST

primary_expression:
	IDENTIFIER {Symbol *sym = SymLookup($1); if (sym == NULL) {
		char err[384];
		sprintf(err, "Symbol not found: %s", $1);
		yyerror(err);
		YYABORT;
	} else $$ = PURE_EXPR(sym);}
	| constant {$$ = PURE_EXPR(GenTemp()); Emit(Mov(ASym($$), AImm($1)));}
	| STRING_LITERAL {$$ = PURE_EXPR(StringLookupOrInsert($1)); free($1);}
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
		DestroyArgList($3);
	}
	| postfix_expression '(' ')' {
		// TODO: call, validate number and type of params
		log("postfix-expression")
	}
	| postfix_expression ARROW IDENTIFIER {
		// TODO: not sure what to do here? we don't have structs
		log("postfix-expression")
	}

argument_expression_list:
	assignment_expression {$$ = MakeArgList(ARG_EXPR($1));}
	| argument_expression_list ',' assignment_expression {InsertArg($1, ARG_EXPR($3)); $$ = $1;}

// TODO: write out relational actions using dummy keys

unary_expression:
	postfix_expression
	| '&' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(UnaryOp(ADDR, ASym($$), ASym($2)));}
	| '*' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(UnaryOp(DEREF, ASym($$), ASym($2)));}
	| '+' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(UnaryOp(POS, ASym($$), ASym($2)));}
	| '-' unary_expression {$$ = PURE_EXPR(GenTemp()); Emit(UnaryOp(NEG, ASym($$), ASym($2)));}
	| '!' unary_expression {$$ = $2;}

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
	type_specifier init_declarator ';' {
		$$ = $2;

		switch ($$->type.kind) {
			case PRIMITIVE_T:
				$$->type.primitive = $1;
			case PRIMITIVE_PTR:
				$$->type.primitive = $1;
			break;

			case ARRAY_T:
				$$->type
			break;

			case ARRAY_PTR:

			break;

			case FUNC_T:

			break;
		}
	}

init_declarator:
	declarator
	| declarator '=' initializer {
		$$ = $1;

		if ($$->type.kind == FUNC_T) {
			yyerror("function can't be initialized with assignment expression");
			YYABORT;
		}

		// TODO: validate type and emit assignment
	}
	
type_specifier:
	VOID {$$ = VOID_T;}
	| CHAR {$$ = CHAR_T;}
	| INT {$$ = INT_T;}

declarator:
	pointer direct_declarator {
		$$ = $2;

		switch ($$->type.kind) {
			case PRIMITIVE_T:
				$$->type.kind = PRIMITIVE_PTR;
			break;

			case ARRAY_T:
				$$->type.kind = ARRAY_PTR;
			break;

			case  FUNC_T:
				$$->type.func.return_type->kind = PRIMITIVE_PTR;
			break;
		}
	}
	| direct_declarator

direct_declarator:
	IDENTIFIER {
		$$ = SymInit(PRIMITIVE_T);
		$$->name = strdup($1);
	}
	| IDENTIFIER '[' INTCONST ']' {
		$$ = SymInit(ARRAY_T);
		$$->type.array.size = $3;
		$$->name = strdup($1);
	}
	| IDENTIFIER '(' parameter_list ')' {
		$$ = SymInit(FUNC_T);
		$$->type.func.arg_list = $3;
		$$->type.func.return_type = calloc(1, sizeof(*$$->type.func.return_type));
		$$->name = strdup($1);
	}
	| IDENTIFIER '(' ')' {
		$$ = SymInit(FUNC_T);
		$$->type.func.arg_list = NULL;
		$$->type.func.return_type = calloc(1, sizeof(*$$->type.func.return_type));
		$$->name = strdup($1);
	}

pointer:
	'*'

parameter_list:
	parameter_declaration {$$ = MakeArgList($1);}
	| parameter_list ',' parameter_declaration {InsertArg($1, $3); $$ = $1;}
	
parameter_declaration:
	type_specifier pointer IDENTIFIER {
		char *name = strdup($3);
		$$ = ARG_DECL(prim2type($1), name); $$.kind = PRIMITIVE_PTR;
	}
	| type_specifier IDENTIFIER {if($1 == VOID_T) {yyerror("void is zero-sized!"); YYABORT;} $$ = prim2type($1);}
	| type_specifier pointer {$$ = prim2type($1); $$.kind = PRIMITIVE_PTR;}
	| type_specifier {if($1 == VOID_T) {yyerror("void is zero-sized!"); YYABORT;} $$ = prim2type($1);}

initializer:
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