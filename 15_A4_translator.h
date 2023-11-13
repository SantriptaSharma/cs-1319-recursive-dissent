#ifndef _TRANSLATOR_H_
#define _TRANSLATOR_H_

const unsigned int size_of_char = 1;
const unsigned int size_of_int = 4;
const unsigned int size_of_pointer = 4;

typedef struct {
	enum OPCODE {ADD, SUB, MUL, DIV, MOD, POS, NEG, PTR, JMP, JIF, JNT, PAR, CAL, RET, INDR, INDW, ADDR, DEREF, PTRW} opcode;
	int arg1, arg2, arg3;
} Quad;


extern int quads_size, quads_capacity;
extern Quad *quads;

void Emit(Quad q);

struct _Symbol;

typedef struct _SymbolTable {
	const char *name;
	enum Scope {GLOBAL, FUNC, BLOCK} scope;
	struct _SymbolTable *parent;
	struct _Symbol *sym_head, *sym_tail;
} SymbolTable;

extern SymbolTable glb_table, *current_table;

SymbolTable *Create_SymbolTable(const char *name, enum Scope scope, SymbolTable *parent);
void Destroy_SymbolTable(SymbolTable *table);
void SymTableDispl(SymbolTable *table);

typedef enum {CHAR, INT} PRIMITIVE_TYPE;

typedef struct _Type {
	// needs some kind of types table + hashing mechanism to reuse types, instead of always creating new ones (wont implement for now)
	enum KIND_T {PRIMITIVE_T, ARRAY_T, FUNC_T} kind;
	union {
		PRIMITIVE_TYPE primitive;
		struct {
			struct _Type *base;
			int size;
		} array;
		struct {
			struct _Type *return_type;
			struct _Type *param_list;
		} func;
	};
	struct _Type *next;
} Type;

void TypeFree(Type *type);

typedef struct _Symbol {
	const char *name;
	Type type;
	int initial_value;
	int size;
	int offset;
	SymbolTable *inner_table;
	struct _Symbol *next;
} Symbol;

Symbol *SymLookup(const char *name);
Symbol *GenTemp();
void SymInsert(Symbol *sym);
void SymFree(Symbol *sym);
void SymDispl(Symbol *sym);

#endif