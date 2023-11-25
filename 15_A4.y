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

%union {
	// Attribute for all expressions, contains pointers to a symbol, and optional pointers to true/false lists (non-null for bool exprs)
	ExprAttrib expr;

	// Attribute for all declarations, contains a pointer to a symbol (which we fill in slowly), and optionally an initialiser
	struct _decl_attrib {
		Symbol *sym;
		ExprAttrib init;
		char has_init;
	} decl;

	// Attribute for list of arguments, used for both parameter declarations and calls
	ArgList *argument_list;
	// A single argument, either a declaration (for params) or an expression (for call args)
	ArgListElem arg;

	// Attribute for an optional_expression, contains the expression attribute and a flag indicating whether it's present
	struct _opt_expr {
		ExprAttrib expr;
		char has_expr;
	} opt_expr;

	// Attribute for type specifier (int, char, void) tokens, just an enum mapping to INT_T, CHAR_T, VOID_T
	PRIMITIVE_TYPE type_spec;

	// Attribute for statements & guards, contains a pointer to their dangling exit quad addresses
	QuadList *next_list;

	// Attribute for a marker, contains the number of quads emitted at the time of the marker's reduction
	size_t quad_index;
	
	// Attribute for constants
	char *string;
	int val;
}

/* %token KEYWORD */

%token <string> IDENTIFIER
%token <val> INTCONST
%token <val> CHARCONST
%token <string> STRING_LITERAL

/* %token PUNCTUATOR */
/* %token ARROW */
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

%type <next_list> statement
%type <next_list> compound_statement
%type <next_list> block_item_list
%type <next_list> block_item
%type <opt_expr> opt_expression
%type <next_list> expression_statement
%type <next_list> selection_statement
%type <next_list> iteration_statement
%type <next_list> jump_statement

%type <quad_index> marker
%type <next_list> guard

%start translation_unit

%%
/* auxiliary symbols */
marker: { $$ = quads_size; } // just store the quad index of the next instruction on reduction
guard: { $$ = MakeList(quads_size); Emit(Jump(AImm(0))); } // emit an unconditional jump, and store a next list to be filled in by rules that use guards

/* expressions */
/* TODO: validate types and add implicit conversions, on every operation and assignment (wont do) */
constant:
	INTCONST
	| CHARCONST

primary_expression:
	IDENTIFIER {Symbol *sym = SymLookup($1, 1); if (sym == NULL) {
			char err[384];
			sprintf(err, "Symbol not found: %s", $1);
			yyerror(err);
			YYABORT;
		} else $$ = PURE_EXPR(sym);
		free($1);
	}
	| constant {$$ = PURE_EXPR(GenTemp()); $$.sym->initial_value = $1;}
	| STRING_LITERAL {$$ = PURE_EXPR(StringLookupOrInsert($1)); free($1);}
	| '(' expression ')' {$$ = $2;}

postfix_expression:
	primary_expression
	| postfix_expression '[' expression ']' {
		if ($1.sym->type.kind != ARRAY_PTR && $1.sym->type.kind != PRIMITIVE_PTR && $1.sym->type.kind != ARRAY_T) {
			yyerror("can't index non-array type");
			YYABORT;
		}

		enum KIND_T kind = $1.sym->type.kind == ARRAY_PTR ? PRIMITIVE_PTR : PRIMITIVE_T;
		$$ = PURE_EXPR(SymInit(kind));
		size_t len = strlen($1.sym->name) + 15;
		char *name = malloc(len);
		sprintf(name, "%s__[%d]", $1.sym->name, current_table->temp_count++);
		$$.sym->name = name;
		$$.sym->type = $1.sym->type.kind == PRIMITIVE_PTR ? prim2type($1.sym->type.primitive) : prim2type($1.sym->type.array.base);
		$$.sym->size = GetSize($$.sym->type);

		SymInsert($$.sym);

		Emit(IndexRead(ASym($$), ASym($1), ASym($3)));
	}
	| postfix_expression '(' argument_expression_list ')' {
		// TODO: validate type of params (wont do)
		if ($1.sym->type.kind != FUNC_T) {
			yyerror("can't call non-function type");
			YYABORT;
		}

		if ($1.sym->type.func.arg_list == NULL) {
			yyerror("function does not take any parameters");
			YYABORT;
		}

		size_t argc = 0;

		ArgList *it = $1.sym->type.func.arg_list;
		ArgList *it2 = $3;

		while (it != NULL && it2 != NULL) {
			Emit(Param(ASym(it2->elem.expr)));

			argc += 1;
			it = it->next;
			it2 = it2->next;
		}

		if (it != NULL || it2 != NULL) {
			yyerror("function call has wrong number of parameters");
			YYABORT;
		}

		DestroyArgList($3);
		
		$$ = PURE_EXPR(GenTemp());

		Emit(CallAss(ASym($$), ASym($1), AImm(argc)));
	}
	| postfix_expression '(' ')' {
		if ($1.sym->type.kind != FUNC_T) {
			yyerror("can't call non-function type");
			YYABORT;
		}

		if ($1.sym->type.func.arg_list != NULL) {
			yyerror("function does not take 0 parameters");
			YYABORT;
		}
		
		$$ = PURE_EXPR(GenTemp());

		Emit(CallAss(ASym($$), ASym($1), AImm(0)));
	}
	/* | postfix_expression ARROW IDENTIFIER {
		// not sure what to do here? we don't have structs
		log("postfix-expression")
	} */

argument_expression_list:
	assignment_expression {$$ = MakeArgList(ARG_EXPR($1));}
	| argument_expression_list ',' assignment_expression {InsertArg($1, ARG_EXPR($3)); $$ = $1;}

unary_expression:
	postfix_expression
	| '&' unary_expression {
		if ($2.sym->type.kind == FUNC_T || $2.sym->type.kind == ARRAY_PTR || $2.sym->type.kind == PRIMITIVE_PTR) {
			yyerror("can't take address of func or a pointer, no multidim pointers");
			YYABORT;
		}

		$$ = PURE_EXPR(GenTemp()); 
		$$.sym->type.kind = $2.sym->type.kind == ARRAY_T ? ARRAY_PTR : PRIMITIVE_PTR;

		if ($2.sym->type.kind == ARRAY_T) {
			$$.sym->type.array.base = $2.sym->type.array.base;
		} else {
			$$.sym->type.primitive = $2.sym->type.primitive;
		}

		$$.sym->size = GetSize($$.sym->type);

		Emit(UnaryOp(ADDR, ASym($$), ASym($2)));
	}
	| '*' unary_expression {
		if ($2.sym->type.kind != ARRAY_PTR && $2.sym->type.kind != PRIMITIVE_PTR && $2.sym->type.kind != ARRAY_T) {
			yyerror("can't dereference non-pointer type");
			YYABORT;
		}
		
		enum KIND_T kind = $2.sym->type.kind == ARRAY_PTR ? ARRAY_T : PRIMITIVE_T;
		size_t len = strlen($2.sym->name) + 4;
		char *name = malloc(len);
		sprintf(name, "*__%s", $2.sym->name);

		$$ = PURE_EXPR(SymInit(kind));

		$$.sym = SymLookup(name, 1);

		if ($$.sym != NULL) {
			free(name);
		} else {
			$$ = PURE_EXPR(SymInit(kind));
			$$.sym->name = name;
			$$.sym->type = $2.sym->type.kind != PRIMITIVE_T ? prim2type($2.sym->type.array.base) : prim2type($2.sym->type.primitive);
			$$.sym->size = GetSize($$.sym->type);

			SymInsert($$.sym);
		}

		Emit(UnaryOp(DEREF, ASym($$), ASym($2)));
	}
	| '+' unary_expression {
		$$ = PURE_EXPR(GenTemp());

		if ($2.sym->initial_value != 0) {
			$$.sym->initial_value = $2.sym->initial_value;
		} else {
			Emit(UnaryOp(POS, ASym($$), ASym($2)));
		}
	}
	| '-' unary_expression {
		$$ = PURE_EXPR(GenTemp()); 
		
		if ($2.sym->initial_value != 0) {
			$$.sym->initial_value = -$2.sym->initial_value;
		} else {
			Emit(UnaryOp(NEG, ASym($$), ASym($2)));
		}
	}
	| '!' unary_expression {
		if ($2.truelist == NULL) {
			$2.truelist = MakeList(quads_size);
			Emit(JumpIf(AImm(0), ASym($2)));
			$2.falselist = MakeList(quads_size);
			Emit(Jump(AImm(0)));
		}

		$$ = BOOL_EXPR($2.sym, $2.falselist, $2.truelist);
	}

multiplicative_expression:
	unary_expression
	| multiplicative_expression '*' unary_expression {
		$$ = PURE_EXPR(GenTemp());

		// if (!TypeEqual($1.sym->type, $3.sym->type)) {
			
		// 	if (TypeCompatible($1.sym->type, $3.sym->type)) {
		// 		$3.sym = Cast($3.sym, $1.sym->type);
		// 	}
		
		// 	yyerror("can't multiply different types");
		// 	YYABORT;
		// }

		if ($1.sym->initial_value != 0 && $3.sym->initial_value != 0) {
			$$.sym->initial_value = $1.sym->initial_value * $3.sym->initial_value;
		} else {
			Emit(BinOp(MUL, ASym($$), ASym($1), ASym($3)));
		}
	}
	| multiplicative_expression '/' unary_expression {
		$$ = PURE_EXPR(GenTemp()); 
		
		if ($1.sym->initial_value != 0 && $3.sym->initial_value != 0) {
			$$.sym->initial_value = $1.sym->initial_value / $3.sym->initial_value;
		} else {
			Emit(BinOp(DIV, ASym($$), ASym($1), ASym($3)));
		}
	}
	| multiplicative_expression '%' unary_expression {
		$$ = PURE_EXPR(GenTemp()); 
		
		if ($1.sym->initial_value != 0 && $3.sym->initial_value != 0) {
			$$.sym->initial_value = $1.sym->initial_value % $3.sym->initial_value;
		} else {
			Emit(BinOp(MOD, ASym($$), ASym($1), ASym($3)));
		}		
	}

additive_expression:
	multiplicative_expression
	| additive_expression '+' multiplicative_expression {
		$$ = PURE_EXPR(GenTemp()); 
		
		if ($1.sym->initial_value != 0 && $3.sym->initial_value != 0) {
			$$.sym->initial_value = $1.sym->initial_value + $3.sym->initial_value;
		} else {
			Emit(BinOp(ADD, ASym($$), ASym($1), ASym($3)));
		}
	}
	| additive_expression '-' multiplicative_expression {
		$$ = PURE_EXPR(GenTemp()); 
	
		if ($1.sym->initial_value != 0 && $3.sym->initial_value != 0) {
			$$.sym->initial_value = $1.sym->initial_value - $3.sym->initial_value;
		} else {
			Emit(BinOp(SUB, ASym($$), ASym($1), ASym($3)));
		}
	}

relational_expression:
	additive_expression
	| relational_expression '<' additive_expression {
		QuadList *tl = MakeList(quads_size);
		Emit(JumpIfLess(AImm(0), ASym($1), ASym($3)));
		QuadList *fl = MakeList(quads_size);
		Emit(Jump(AImm(0)));
		$$ = BOOL_EXPR($1.sym, tl, fl);
	}
	| relational_expression '>' additive_expression {
		QuadList *tl = MakeList(quads_size);
		Emit(JumpIfGreater(AImm(0), ASym($1), ASym($3)));
		QuadList *fl = MakeList(quads_size);
		Emit(Jump(AImm(0)));

		$$ = BOOL_EXPR($1.sym, tl, fl);
	}
	| relational_expression LEQ additive_expression {
		QuadList *tl = MakeList(quads_size);
		Emit(JumpIfLessEqual(AImm(0), ASym($1), ASym($3)));
		QuadList *fl = MakeList(quads_size);
		Emit(Jump(AImm(0)));

		$$ = BOOL_EXPR($1.sym, tl, fl);
	}
	| relational_expression GEQ additive_expression {
		QuadList *tl = MakeList(quads_size);
		Emit(JumpIfGreaterEqual(AImm(0), ASym($1), ASym($3)));
		QuadList *fl = MakeList(quads_size);
		Emit(Jump(AImm(0)));

		$$ = BOOL_EXPR($1.sym, tl, fl);
	}

equality_expression:
	relational_expression
	| equality_expression CEQ relational_expression {
		QuadList *tl = MakeList(quads_size);
		Emit(JumpIfEqual(AImm(0), ASym($1), ASym($3)));
		QuadList *fl = MakeList(quads_size);
		Emit(Jump(AImm(0)));

		$$ = BOOL_EXPR($1.sym, tl, fl);
	}
	| equality_expression NEQ relational_expression {
		QuadList *tl = MakeList(quads_size);
		Emit(JumpIfNotEqual(AImm(0), ASym($1), ASym($3)));
		QuadList *fl = MakeList(quads_size);
		Emit(Jump(AImm(0)));

		$$ = BOOL_EXPR($1.sym, tl, fl);
	}

logical_AND_expression:
	equality_expression
	| logical_AND_expression {
		if ($1.truelist == NULL) {
			$1.truelist = MakeList(quads_size);
			Emit(JumpIf(AImm(0), ASym($1)));
			$1.falselist = MakeList(quads_size);
			Emit(Jump(AImm(0)));
		}
	} LAND marker equality_expression {
		if ($5.truelist == NULL) {
			$5.truelist = MakeList(quads_size);
			Emit(JumpIf(AImm(0), ASym($5)));
			$5.falselist = MakeList(quads_size);
			Emit(Jump(AImm(0)));
		}

		Backpatch($1.truelist, $4);

		$$ = BOOL_EXPR($1.sym, $5.truelist, Merge($1.falselist, $5.falselist));
	}

logical_OR_expression:
	logical_AND_expression
	| logical_OR_expression {
		if ($1.truelist == NULL) {
			$1.truelist = MakeList(quads_size);
			Emit(JumpIf(AImm(0), ASym($1)));
			$1.falselist = MakeList(quads_size);
			Emit(Jump(AImm(0)));
		}
	} LOR marker logical_AND_expression {
		if ($5.truelist == NULL) {
			$5.truelist = MakeList(quads_size);
			Emit(JumpIf(AImm(0), ASym($5)));
			$5.falselist = MakeList(quads_size);
			Emit(Jump(AImm(0)));
		}

		Backpatch($1.falselist, $4);

		$$ = BOOL_EXPR($1.sym, Merge($1.truelist, $5.truelist), $5.falselist);
	}

conditional_expression:
	logical_OR_expression
	| logical_OR_expression {
		if ($1.truelist == NULL) {
			$1.truelist = MakeList(quads_size);
			Emit(JumpIf(AImm(0), ASym($1)));
			$1.falselist = MakeList(quads_size);
			Emit(Jump(AImm(0)));
		}
	} '?' marker expression guard ':' marker conditional_expression guard {
		Backpatch($1.truelist, $4); 
		Backpatch($1.falselist, $8);

		$$ = PURE_EXPR(GenTemp());
		Backpatch($6, quads_size);
		Emit(Mov(ASym($$), ASym($5)));
		size_t q = quads_size;
		Emit(Jump(AImm(0)));

		Backpatch($10, quads_size);
		Emit(Mov(ASym($$), ASym($9)));
		quads[q].rd.imm = quads_size;
	}

assignment_expression:
	conditional_expression
	| unary_expression '=' assignment_expression {
		if ($1.sym->type.kind == FUNC_T || $1.sym->is_temp) {
			yyerror("functions & temps can't be assigned to");
			YYABORT;
		}

		Emit(Mov(ASym($1), ASym($3)));
		$$ = $1;
	}

expression:
	assignment_expression

/* Declarations */
declaration:
	type_specifier init_declarator ';' {
		$$ = $2;

		switch ($$.sym->type.kind) {
			case PRIMITIVE_T:
			case PRIMITIVE_PTR:
				$$.sym->type.primitive = $1;
			break;

			case ARRAY_T:
			case ARRAY_PTR:
				$$.sym->type.array.base = $1;
			break;

			case FUNC_T:
				$$.sym->type.func.return_type->primitive = $1;
				
				current_table = $$.sym->inner_table;

				Symbol *sym = SymInit($$.sym->type.func.return_type->kind);
				sym->name = strdup("__retval");
				sym->type = *$$.sym->type.func.return_type;
				sym->size = GetSize(sym->type);

				SymInsert(sym);

				current_table = &glb_table;
			break;
		}

		if ($1 == VOID_T && $$.sym->type.kind != FUNC_T) {
			yyerror("void is zero-sized!");
			YYABORT;
		}

		Symbol *existing = SymLookup($$.sym->name, 0);
		$$.sym->size = GetSize($$.sym->type);

		if (existing != NULL && $$.sym->type.kind != FUNC_T) {
			char err[384];
			sprintf(err, "redeclaration of symbol %s", $$.sym->name);
			yyerror(err);
			YYABORT;
		} else if (existing != NULL) {
			// TODO: validate signature against existing entry (wont do)
			SymFree($$.sym);
			$$.sym = existing;
		} else {
			SymInsert($$.sym);
		}


		// TODO: verify types before emitting assignment (wont do)
		if ($$.has_init == 1) {
			Emit(Mov(ASym($$), ASym($$.init)));
		}
	}

init_declarator:
	declarator
	| declarator '=' initializer {
		$$ = $1;

		if ($$.sym->type.kind == FUNC_T) {
			yyerror("function can't be initialized with assignment expression");
			YYABORT;
		}

		$$.init = $3;
		$$.has_init = 1;
	}
	
type_specifier:
	VOID {$$ = VOID_T;}
	| CHAR {$$ = CHAR_T;}
	| INT {$$ = INT_T;}

declarator:
	pointer direct_declarator {
		$$ = $2;

		switch ($$.sym->type.kind) {
			case PRIMITIVE_T:
				$$.sym->type.kind = PRIMITIVE_PTR;
			break;

			case ARRAY_T:
				$$.sym->type.kind = ARRAY_PTR;
			break;

			case  FUNC_T:
				$$.sym->type.func.return_type->kind = PRIMITIVE_PTR;
			break;
		}
	}
	| direct_declarator

direct_declarator:
	IDENTIFIER {
		$$.sym = SymInit(PRIMITIVE_T);
		$$.sym->name = $1;
	}
	| IDENTIFIER '[' INTCONST ']' {
		$$.sym = SymInit(ARRAY_T);
		$$.sym->type.array.size = $3;
		$$.sym->name = $1;
	}
	| IDENTIFIER '(' parameter_list ')' {
		if (current_table != &glb_table) {
			yyerror("function declaration/definition outside of global scope");
			YYABORT;
		}

		$$.sym = SymInit(FUNC_T);
		$$.sym->type.func.arg_list = $3;
		$$.sym->type.func.return_type = calloc(1, sizeof(*$$.sym->type.func.return_type));
		*$$.sym->type.func.return_type = prim2type(INT_T);
		$$.sym->name = $1;
		$$.sym->inner_table = Create_SymbolTable($$.sym->name, FUNC, &glb_table);
;
		current_table = $$.sym->inner_table;

		ArgList *it = $$.sym->type.func.arg_list;

		while (it != NULL) {
			Symbol *sym = SymInit(it->elem.decl.type.kind);

			sym->name = it->elem.decl.name;
			sym->type = it->elem.decl.type;
			sym->size = GetSize(sym->type);

			SymInsert(sym);

			it = it->next;
		}

		current_table = &glb_table;
	}
	| IDENTIFIER '(' ')' {
		if (current_table != &glb_table) {
			yyerror("function declaration/definition outside of global scope");
			YYABORT;
		}

		$$.sym = SymInit(FUNC_T);
		$$.sym->type.func.arg_list = NULL;
		$$.sym->type.func.return_type = calloc(1, sizeof(*$$.sym->type.func.return_type));
		*$$.sym->type.func.return_type = prim2type(INT_T);
		$$.sym->name = $1;
		$$.sym->inner_table = Create_SymbolTable($$.sym->name, FUNC, &glb_table);
	}

pointer:
	'*'

parameter_list:
	parameter_declaration {$$ = MakeArgList($1);}
	| parameter_list ',' parameter_declaration {InsertArg($1, $3); $$ = $1;}
	
parameter_declaration:
	type_specifier pointer IDENTIFIER {
		char *name = $3;
		$$ = ARG_DECL(prim2type($1), name);
		$$.decl.type.kind = PRIMITIVE_PTR;
	}
	| type_specifier IDENTIFIER {
		if($1 == VOID_T) {yyerror("void is zero-sized!"); YYABORT;} 
		char *name = $2;
		$$ = ARG_DECL(prim2type($1), name);
	}

initializer:
	assignment_expression

/* Statements */
statement:
	compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement

compound_statement:
	'{' '}' { $$ = NULL; }
	| '{' block_item_list '}' { $$ = $2; }
	
block_item_list:
	block_item
	| block_item_list marker block_item { Backpatch($1, $2); $$ = $3; Backpatch($$, quads_size); }

block_item:
	declaration { $$ = NULL; }
	| statement

opt_expression:
	expression { $$.expr = $1; $$.has_expr = 1; }
	| /* empty */ { $$.has_expr = 0; }

expression_statement:
	opt_expression ';' { $$ = NULL; }

selection_statement:
	IF '(' expression ')' marker statement { 
		if ($3.truelist != NULL) {
			Backpatch($3.truelist, $5); $$ = Merge($3.falselist, $6); 
		} else {
			Emit(JumpIf(AImm($5), ASym($3)));
			$$ = MakeList(quads_size);
			Emit(Jump(AImm(0)));
		}
	}
	| IF '(' expression ')' marker statement guard ELSE marker statement { 
		if ($3.truelist != NULL) {
			Backpatch($3.truelist, $5); Backpatch($3.falselist, $9);
			$$ = Merge($6, $7); $$ = Merge($$, $10);
		} else {
			Emit(JumpIf(AImm($5), ASym($3)));
			Emit(Jump(AImm($9)));
			$$ = Merge($6, $10);
		}
	}

iteration_statement:
	FOR '(' opt_expression ';' marker opt_expression ';' marker opt_expression guard ')' marker statement {
		QuadList *tl = $6.has_expr ? $6.expr.truelist : NULL;
		QuadList *fl = $6.has_expr ? $6.expr.falselist : NULL;

		Backpatch(tl, $12); Backpatch($10, $5); Backpatch($13, $8);
		Emit(Jump(AImm($8)));
		$$ = fl;
	}
	
jump_statement:
	RETURN opt_expression ';' { 
		if (current_table->scope != FUNC) {
			yyerror("return statement outside of function");
			YYABORT;
		} 
		
		$$ = NULL;

		if ($2.has_expr == 1) {
			Emit(Mov(((Addr){SYMBOL_A, .sym = SymLookup("__retval", 0)}), ASym($2.expr)));
		}

		Emit(Return(AImm(0)));
	}

/* TLU */
translation_unit:
	external_declaration
	| translation_unit external_declaration

external_declaration:
	function_definition
	| declaration
	
function_definition:
	type_specifier declarator {
		if ($2.sym->type.kind != FUNC_T) {
			yyerror("function definition must be a function");
			YYABORT;
		}

		$2.sym->type.func.return_type->primitive = $1;
		
		current_table = $2.sym->inner_table;

		Symbol *sym = SymInit($2.sym->type.func.return_type->kind);
		sym->name = strdup("__retval");
		sym->type = *$2.sym->type.func.return_type;
		sym->size = GetSize(sym->type);

		SymInsert(sym);

		current_table = &glb_table;

		Symbol *existing = SymLookup($2.sym->name, 0);

		if (existing != NULL) {
			// TODO: validate signature against existing entry (wont do)
			SymFree($2.sym);
			$2.sym = existing;
		} else {
			SymInsert($2.sym);
		}

		current_table = $2.sym->inner_table;

		Emit(FnLabel(ASym($2)));
	} compound_statement {
		current_table = &glb_table;

		if (quads[quads_size - 1].opcode != RET) {
			Backpatch($4, quads_size);
			Emit(Return(AImm(0)));
		} else {
			Backpatch($4, quads_size - 1);
		}
	}
%%

void yyerror(char *s) {
	printf("Error: %s on '%s'\n", s, yytext);
}