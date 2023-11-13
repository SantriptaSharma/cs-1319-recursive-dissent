#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "15_A4_translator.h"

extern int yyparse();
extern void yyerror(const char *err);

int quads_size = 0, quads_capacity = 64;
Quad *quads;

SymbolTable glb_table, *current_table;


static void InitQuads()
{
	quads = calloc(quads_capacity, sizeof(*quads));
}

static void FreeQuads()
{
	free(quads);
}

void Emit(Quad q)
{
	if (quads_size == quads_capacity) {
		quads_capacity *= 2;
		quads = realloc(quads, quads_capacity * sizeof(*quads));
	}

	quads[quads_size++] = q;
}

static void InitTables()
{
	glb_table = (SymbolTable) {
		.name = "global",
		.scope = GLOBAL,
		.parent = NULL,
		.sym_head = NULL,
		.sym_tail = NULL
	};

	current_table = &glb_table;
}

static void FreeTables()
{
	Destroy_SymbolTable(&glb_table);
}

SymbolTable *Create_SymbolTable(const char *name, enum Scope scope, SymbolTable *parent)
{
	SymbolTable *table = malloc(sizeof(*table));

	table->name = strdup(name);
	table->scope = scope;
	table->parent = parent;
	table->sym_head = table->sym_tail = NULL;

	return table;
}

void Destroy_SymbolTable(SymbolTable *table)
{
	Symbol *sym = table->sym_head;
	while (sym) {
		Symbol *next = sym->next;
		SymFree(sym);
		sym = next;
	}

	if (table->scope != GLOBAL) free((void *) table->name);
	free(table);
}

void SymTableDispl(SymbolTable *table)
{
	printf("Symbol table %s, ", table->name);
	printf("Parent %s, ", table->parent ? table->parent->name : "NULL");
	printf("Scope %s\n", table->scope == GLOBAL ? "GLOBAL" : table->scope == FUNC ? "FUNC" : "BLOCK");

	Symbol *sym = table->sym_head;
	while (sym) {
		SymDispl(sym);
		sym = sym->next;
	}
}

void TypeFree(Type *type)
{
	if (type->kind == PRIMITIVE_T) return;

	if (type->kind == ARRAY_T) {
		TypeFree(type->array.base);
	} else if (type->kind == FUNC_T) {
		TypeFree(type->func.return_type);
		
		Type *param = type->func.param_list;
		while (param) {
			Type *next = param->next;
			TypeFree(param);
			param = next;
		}
	}
}

Symbol *SymLookup(const char *name)
{
	Symbol *sym = current_table->sym_head;
	while (sym) {
		if (!strcmp(sym->name, name))
			return sym;

		sym = sym->next;
	}

	return NULL;
}

Symbol *GenTemp()
{
	static int temp_count = 0;
	char name[16];
	sprintf(name, "t%d", temp_count++);

	Symbol *sym = malloc(sizeof(*sym));
	
	sym->name = strdup(name);
	sym->type = (Type) {
		.kind = PRIMITIVE_T,
		.primitive = INT
	};
	sym->initial_value = 0;
	sym->size = size_of_int;
	sym->offset = 0;
	sym->inner_table = NULL;
	sym->next = NULL;

	return sym;
}

void SymInsert(Symbol *sym)
{
	if (current_table->sym_head == NULL) {
		current_table->sym_head = current_table->sym_tail = sym;
	} else {
		current_table->sym_tail->next = sym;
		current_table->sym_tail = sym;
	}
}

void SymFree(Symbol *sym)
{
	free((void *) sym->name);

	if (sym->inner_table) Destroy_SymbolTable(sym->inner_table);
	TypeFree(&sym->type);

	free(sym);
}

void SymDispl(Symbol *sym)
{
	printf("Symbol %s: ", sym->name);
	printf("%s, ", sym->type.kind == PRIMITIVE_T ? "PRIMITIVE" : sym->type.kind == ARRAY_T ? "ARRAY" : "FUNC");
	printf("Initial value %d, ", sym->initial_value);
	printf("%d bytes, ", sym->size);
	printf("@%d, ", sym->offset);
	printf("Inner table %s\n", sym->inner_table ? sym->inner_table->name : "NULL");
}


int main() {
	InitQuads();
	InitTables();

	SymTableDispl(current_table);

	yyparse();
	
	FreeTables();
	FreeQuads();
	return 0;
}