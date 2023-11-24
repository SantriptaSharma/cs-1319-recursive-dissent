#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "15_A4_translator.h"

extern int yyparse();
extern void yyerror(char *s);

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
	[ADD] "+", [SUB] "-", [MUL] "*", [DIV] "/", [MOD] "%",
	[POS] "+", [NEG] "-", [ADDR] "&", [DEREF] "*", [JLT] "<",
	[JGT] ">", [JEQ] "==", [JNE] "!=", [JLE] "<=", [JGE] ">=",
};

static const int Sizes[] = {
	[INT_T] size_of_int, [CHAR_T] size_of_char, [VOID_T] 0
};

int existing_lists_size = 0, existing_lists_capacity = 64;
QuadList **existing_lists;

void InitLists() {
	existing_lists = calloc(existing_lists_capacity, sizeof(*existing_lists));
}

void AddList(QuadList *list) {
	if (existing_lists_size == existing_lists_capacity) {
		existing_lists_capacity *= 2;
		existing_lists = realloc(existing_lists, existing_lists_capacity * sizeof(*existing_lists));
	}

	existing_lists[existing_lists_size++] = list;
}

void DestroyLists() {
	for (int i = 0; i < existing_lists_size; i++) {
		FreeList(existing_lists[i]);
	}

	free(existing_lists);
}

QuadList *MakeList(int ind) {
	QuadList *list = malloc(sizeof(*list));
	list->quad_index = ind;
	list->next = NULL;

	AddList(list);

	return list;
}

void Insert(QuadList *list, int ind) {
	QuadList *new = malloc(sizeof(*new));
	new->quad_index = ind;
	new->next = list->next;
	list->next = new;
}

QuadList *Merge(QuadList *list1, QuadList *list2) {
	// could be in place but im the RAM devil 😈
	QuadList *new = MakeList(-1);

	QuadList *it = list1;
	while (it != NULL) {
		Insert(new, it->quad_index);
		it = it->next;
	}

	it = list2;

	while (it != NULL) {
		Insert(new, it->quad_index);
		it = it->next;
	}

	return new;
}

void Backpatch(QuadList *list, int dest) {
	QuadList *it = list;
	while (it != NULL) {
		if (it->quad_index != -1) quads[it->quad_index].rd.imm = dest;
		it = it->next;
	}
}

void FreeList(QuadList *list) {
	QuadList *it = list;
	while (it != NULL) {
		QuadList *next = it->next;
		free(it);
		it = next;
	}
}

void Emit(Quad q) {
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
			printf("goto %d", q.rd.imm);
		break;
		case JIF:
			printf("if ");
			DisplayAddr(q.rs);
			printf(" goto %d", q.rd.imm);
		break;
		case JNT:
			printf("ifFalse ");
			DisplayAddr(q.rs);
			printf(" goto %d", q.rd.imm);
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
			printf(" goto %d", q.rd.imm);
		break;
		case PAR:
			printf("param ");
			DisplayAddr(q.rs);
		break;
		case CAL:
			if (q.rs.kind == SYMBOL_A)
				printf("%s = call %s, %d", q.rs.sym->name, q.rd.sym->name, q.rt.imm);
			else if (q.rs.kind == IMMEDIATE) {
				printf("call %s, %d", q.rd.sym->name, q.rd.imm);
			}
		break;
		case RET:
			printf("return");
			if (q.rs.kind != IMMEDIATE) {
				printf(" %s", q.rs.sym->name);
			}
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
		case FN_LABEL:
			printf("%s:", q.rd.sym->name);
		break;
	}
}

void DisplayQuads() {
	for (int i = 0; i < quads_size; i++) {
		if (quads[i].opcode == FN_LABEL) printf("\n");
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
	SymbolTable *table = calloc(1, sizeof(*table));

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

	if (table->scope != GLOBAL) {
		free((void *) table->name);
		free(table);
	}
}

void SymTableDispl(SymbolTable *table)
{
	printf("----------------------------------------\n");
	printf("Symbol table %s, ", table->name);
	printf("Parent %s, ", table->parent ? table->parent->name : "NULL");
	printf("Scope %s\n", table->scope == GLOBAL ? "GLOBAL" : table->scope == FUNC ? "FUNC" : "BLOCK");

	Symbol *sym = table->sym_head;
	while (sym) {
		SymDispl(sym);
		sym = sym->next;
	}

	printf("----------------------------------------\n");

	sym = table->sym_head;
	while (sym) {
		if (sym->inner_table) SymTableDispl(sym->inner_table);
		sym = sym->next;
	}
}

// Get size of an object of this type on the virtual stack
int GetSize(Type type) {
	switch (type.kind)
	{
		case PRIMITIVE_T:
			return Sizes[type.primitive];
		break;

		case PRIMITIVE_PTR:
		case ARRAY_PTR:
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

char TypeEq(Type t1, Type t2) {
	switch (t1.kind) {
		Type base1, base2;

		case PRIMITIVE_T:
			return t1.primitive == t2.primitive;
		break;

		case PRIMITIVE_PTR:
		case ARRAY_PTR:
		case ARRAY_T:
			base1 = t1.kind == PRIMITIVE_PTR ? prim2type(t1.primitive) : prim2type(t1.array.base);
			base2 = t2.kind == PRIMITIVE_PTR ? prim2type(t2.primitive) : prim2type(t2.array.base);

			if (t2.kind == PRIMITIVE_PTR || t2.kind == ARRAY_PTR || t2.kind == ARRAY_T)
				return TypeEq(base1, base2);
			else
				return 0;
		break;

		case FUNC_T:
			if (t2.kind != FUNC_T) return 0;

			if (t1.func.return_type->primitive != t2.func.return_type->primitive) return 0;

			ArgList *arg1 = t1.func.arg_list;
			ArgList *arg2 = t2.func.arg_list;

			while (arg1 != NULL && arg2 != NULL) {
				if (!TypeEq(arg1->elem.decl.type, arg2->elem.decl.type)) return 0;

				arg1 = arg1->next;
				arg2 = arg2->next;
			}

			return arg1 == NULL && arg2 == NULL;

		default:
			return 0;
		break;
	}
}

// will implicitly allow conversions char <-> int, int <-> pointer, but not char <-> pointer (technically int <-> char just as invalid, but let's allow it)
char TypeCompatible(Type t1, Type t2) {
	switch (t1.kind) {
		PRIMITIVE_TYPE t;
		
		case PRIMITIVE_T:
			return t2.kind == !FUNC_T;
		break;

		case PRIMITIVE_PTR:
		case ARRAY_PTR:
		case ARRAY_T:
			t = t1.kind == PRIMITIVE_PTR ? t1.primitive : t1.array.base;
			return t2.kind != FUNC_T && t != CHAR_T;
		break;

		default:
			return 0;
		break;
	}
}

void TypeFree(Type *type)
{
	if (type->kind == FUNC_T) {
		TypeFree(type->func.return_type);
		free(type->func.return_type);
		DestroyArgList(type->func.arg_list);
	}
}

static const char *TypeSym[] = {
	[PRIMITIVE_T] "PRIM",
	[PRIMITIVE_PTR] "PRIM*",
	[ARRAY_PTR] "ARR*",
	[ARRAY_T] "ARR",
	[FUNC_T] "FUNC"
};

static const char *PrimitiveSym[] = {
	[INT_T] "INT_T",
	[CHAR_T] "CHAR_T",
	[VOID_T] "VOID_T"
};

void TypeDispl(Type t) {
	printf("%s\t\t", TypeSym[t.kind]);
	ArgList *arg = t.func.arg_list;

	switch (t.kind) {
		case PRIMITIVE_T:
			printf("%s", PrimitiveSym[t.primitive]);
		break;

		case PRIMITIVE_PTR:
			printf("*%s", PrimitiveSym[t.primitive]);
		break;

		case ARRAY_PTR:
			printf("*%s[%d]", PrimitiveSym[t.array.base], t.array.size);
		break;

		case ARRAY_T:
			printf("%s[%d]", PrimitiveSym[t.array.base], t.array.size);
		break;

		case FUNC_T:
			if (arg == NULL) {
				printf("NIL -> %s", PrimitiveSym[t.func.return_type->primitive]);
				break;
			}

			while (arg != NULL) {
				if (arg->elem.decl.type.kind == PRIMITIVE_PTR || arg->elem.decl.type.kind == ARRAY_PTR)
					printf("*");
				printf("%s", PrimitiveSym[arg->elem.decl.type.primitive]);

				arg = arg->next;

				if (arg != NULL) printf(" * ");
			}
			printf("-> %s", PrimitiveSym[t.func.return_type->primitive]);
		break;
	}

	printf("\t");
}

Symbol *SymInit(enum KIND_T kind) {
	Symbol *sym = calloc(1, sizeof(*sym));
	sym->type.kind = kind;
	
	switch (kind) {
		case PRIMITIVE_T:
		case PRIMITIVE_PTR:
		case ARRAY_PTR:
			sym->size = GetSize(sym->type);
		break;

		case ARRAY_T:
			sym->size = GetSize(sym->type);
		break;

		case FUNC_T:
			sym->type.func.return_type = NULL;
			sym->type.func.arg_list = NULL;
		break;
	}

	sym->is_temp = 0;

	return sym;
}

// clone any non-func symbol, doesn't assign a name
Symbol *SymClone(Symbol *sym) {
	if (sym->type.kind == FUNC_T) return NULL;

	Symbol *clone = malloc(sizeof(*clone));
	clone->type = sym->type;
	clone->initial_value = sym->initial_value;
	clone->size = sym->size;
	clone->offset = sym->offset;
	clone->is_temp = sym->is_temp;
	clone->inner_table = sym->inner_table;
	clone->next = NULL;

	SymInsert(clone);

	return clone;
}

// cast any non-func symbol to a given type, creating and returning a new symbol
Symbol *Cast(Symbol *sym, Type type) {
	if (type.kind == FUNC_T) return NULL;
	if (TypeEq(sym->type, type)) return sym;

	Symbol *new = SymClone(sym);
	size_t len = strlen(sym->name) + 16;
	new->name = malloc(len);
	sprintf((char *) new->name, "__cast[%d]_%s", current_table->temp_count++, sym->name);
	new->type = type;
	new->size = GetSize(type);
	new->is_temp = 1;

	SymInsert(new);

	Quad q = (Quad) {
		.opcode = MOV,
		.rd = (Addr) {
			.kind = SYMBOL_A,
			.sym = new
		},
		.rs = (Addr) {
			.kind = SYMBOL_A,
			.sym = sym
		}
	};

	Emit(q);

	return new;
}

Symbol *SymLookup(const char *name) {
	Symbol *sym = current_table->sym_head;
	while (sym) {
		if (!strcmp(sym->name, name))
			return sym;

		sym = sym->next;
	}

	SymbolTable *tab = current_table;
	if (current_table->parent != NULL) {
		current_table = current_table->parent;
		sym = SymLookup(name);
		current_table = tab;
	
		return sym;
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

Symbol *StringLookupOrInsert(const char *str) {
	size_t len = strlen(str);
	char *name = malloc(len + 7);
	
	sprintf(name, "__str_%s", str);

	SymbolTable *c = current_table;
	current_table = &glb_table;

	Symbol *sym = SymLookup(name);

	char new = sym == NULL;

	if (new) {
		sym = malloc(sizeof(*sym));
		
		sym->name = name;
		sym->type = (Type) {
			.kind = ARRAY_T,
			.array = {
				.base = CHAR_T,
				.size = len + 1
			}
		};
		sym->initial_value = 0;
		sym->size = sym->type.array.size;
		sym->offset = 0;
		sym->inner_table = NULL;
		sym->next = NULL;

		SymInsert(sym);
	}

	if (!new) free(name);

	current_table = c;
	return sym;
}

Symbol *GenTemp()
{
	char name[20];
	sprintf(name, "__t_%d_", current_table->temp_count++);

	Symbol *sym = SymLookupOrInsert(name);
	sym->type.kind = PRIMITIVE_T;
	sym->is_temp = 1;
	sym->type.primitive = INT_T;

	sym->size = GetSize(sym->type);

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

	current_table->sym_tail->next = NULL;

	sym->offset = current_table->offset;
	current_table->offset += sym->size;
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
	printf("Symbol %s\t\t", sym->name);
	TypeDispl(sym->type);
	printf("Initial value %d\t\t", sym->initial_value);
	printf("%d bytes\t\t", sym->size);
	printf("@%d\t\t", sym->offset);
	printf("Inner table %s\n", sym->inner_table ? sym->inner_table->name : "NULL");
}

ArgList *MakeArgList(ArgListElem elem) {
	ArgList *list = malloc(sizeof(*list));
	list->elem = elem;
	list->next = NULL;
	return list;
}

void InsertArg(ArgList *list,  ArgListElem elem) {
	while (list->next != NULL) {list = list->next;}
	list->next = MakeArgList(elem);
}

void DestroyArgList(ArgList *list) {
	while (list != NULL) {
		// name is freed by associated symbol

		ArgList *next = list->next;
		free(list);
		list = next;
	}
}

int main() {
	InitLists();
	InitQuads();
	InitTables();

	SymTableDispl(current_table);

	yyparse();
	
	SymTableDispl(current_table);
	DisplayQuads();

	FreeTables();
	FreeQuads();
	DestroyLists();
	return 0;
}