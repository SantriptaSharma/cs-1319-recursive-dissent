#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "15_A4_translator.h"

extern int yyparse();
extern void yyerror(const char *err);

const unsigned int size_of_char = 1;
const unsigned int size_of_int = 4;
const unsigned int size_of_pointer = 4;

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

static const char *OpSym[] = {
	[ADD] "+", [SUB] "-", [MUL] "*", [DIV] "/", [MOD] "%%",
	[POS] "+", [NEG] "-", [ADDR] "&", [DEREF] "*", [JLT] "<",
	[JGT] ">", [JEQ] "==", [JNE] "!=", [JLE] "<=", [JGE] ">=",
};

static const int Sizes[] = {
	[INT_T] size_of_int, [CHAR_T] size_of_char
};

void Emit(Quad q)
{
	if (quads_size == quads_capacity) {
		quads_capacity *= 2;
		quads = realloc(quads, quads_capacity * sizeof(*quads));
	}

	quads[quads_size++] = q;
}

static void DisplayAddr(Addr a) {
	switch (a.kind) {
		case IMMEDIATE: printf("%d", a.imm); break;
		case SYMBOL_A: printf("%s", a.sym->name); break;
	}
}

void DisplayQuad(Quad q) {
	switch (q.opcode) {
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case MOD:
			printf("%s = ", q.rd.sym->name);
			DisplayAddr(q.rs);
			printf(" %s ", OpSym[q.opcode]);
			DisplayAddr(q.rt);
		break;
		case POS:
		case NEG:
		case ADDR:
		case DEREF:
			printf("%s = %s", q.rd.sym->name, OpSym[q.opcode]);
			DisplayAddr(q.rs);
		break;
		case JMP:
			printf("goto %s", q.rd.sym->name);
		break;
		case JIF:
			printf("if ");
			DisplayAddr(q.rs);
			printf(" goto %s", q.rd.sym->name);
		break;
		case JNT:
			printf("ifFalse ");
			DisplayAddr(q.rs);
			printf(" goto %s", q.rd.sym->name);
		break;
		case JLT:
		case JGT:
		case JEQ:
		case JNE:
		case JLE:
		case JGE:
			printf("if ");
			DisplayAddr(q.rs);
			printf(" %s ", OpSym[q.opcode]);
			DisplayAddr(q.rt);
			printf(" goto %s", q.rd.sym->name);
		break;
		case PAR:
			printf("param ");
			DisplayAddr(q.rs);
		break;
		case CAL:
			if (q.rs.kind == SYMBOL_A)
				printf("%s = call %s, %d", q.rs.sym->name, q.rd.sym->name, 10000);
			else if (q.rs.kind == IMMEDIATE) {
				printf("call %s, %d", q.rd.sym->name, 10000);
			}
		break;
		case RET:
			printf("return");
		break;
		case INDR:
			printf("%s = %s[", q.rd.sym->name, q.rs.sym->name);
			DisplayAddr(q.rt);
			printf("]");
		break;
		case INDW:
			printf("%s[", q.rd.sym->name);
			DisplayAddr(q.rs);
			printf("] = ");
			DisplayAddr(q.rt);
		break;
		case PTRW:
			printf("*%s = ", q.rd.sym->name);
			DisplayAddr(q.rs);
		break;
		case MOV:
			printf("%s = ", q.rd.sym->name);
			DisplayAddr(q.rs);
		break;
	}
}

void DisplayQuads() {
	for (int i = 0; i < quads_size; i++) {
		printf("%d: ", i);
		DisplayQuad(quads[i]);
		printf("\n");
	}
	printf("\n");
}

static void InitTables()
{
	glb_table = (SymbolTable) {
		.name = "global",
		.scope = GLOBAL,
		.parent = NULL,
		.sym_head = NULL,
		.sym_tail = NULL,
		.temp_count = 0
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
	table->temp_count = 0;

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

// Get size of an object of this type on the virtual stack
int GetSize(Type type) {
	switch (type.kind)
	{
		case PRIMITIVE_T:
		case TEMP_T:
			return Sizes[type.primitive];
		break;

		case PRIMITIVE_PTR:
			return size_of_pointer;
		break;

		case ARRAY_T:
			return Sizes[type.array.base] * type.array.size;
		break;
		
		case FUNC_T:
			return 0;
		break;

		default:
			return size_of_pointer;
		break;
	}	
}

void TypeFree(Type *type)
{
	if (type->kind == PRIMITIVE_T) return;

	if (type->kind == FUNC_T) {
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

Symbol *SymLookupOrInsert(const char *name) {
	Symbol *sym = SymLookup(name);

	if (sym == NULL) {
		sym = malloc(sizeof(*sym));
		
		sym->name = strdup(name);
		sym->type = (Type) {
			.kind = PRIMITIVE_T,
			.primitive = INT_T
		};
		sym->initial_value = 0;
		sym->size = size_of_int;
		sym->offset = 0;
		sym->inner_table = NULL;
		sym->next = NULL;

		SymInsert(sym);
	}

	return sym;
}

Symbol *GenTemp()
{
	char name[16];
	sprintf(name, "__t_%d_", current_table->temp_count++);

	Symbol *sym = SymLookupOrInsert(name);
	sym->type.kind = TEMP_T;

	sym->size = GetSize(sym->type);
	printf("Generated temp %s of size %d\n", sym->name, sym->size);

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

static const char *TypeSym[] = {
	[PRIMITIVE_T] "PRIMITIVE_T",
	[PRIMITIVE_PTR] "PRIMITIVE_PTR",
	[TEMP_T] "TEMP_T",
	[ARRAY_T] "ARRAY_T",
	[FUNC_T] "FUNC_T"
};

void SymDispl(Symbol *sym)
{
	printf("Symbol %s: ", sym->name);
	printf("%s, ", TypeSym[sym->type.kind]);
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
	
	DisplayQuads();
	SymTableDispl(current_table);

	FreeTables();
	FreeQuads();
	return 0;
}