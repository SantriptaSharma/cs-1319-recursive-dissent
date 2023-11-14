#ifndef _TRANSLATOR_H_
#define _TRANSLATOR_H_

extern const unsigned int size_of_char;
extern const unsigned int size_of_int;
extern const unsigned int size_of_pointer;

struct _Symbol;

typedef struct _Addr {
	enum ADDR_KIND {SYMBOL_A, IMMEDIATE} kind;
	union {
		int imm;
		struct _Symbol *sym;
	};
} Addr;

// TODO: remove not if assignment finally does not ask for it, a couple places to remove from
typedef struct {
	enum OPCODE {ADD, SUB, MUL, DIV, MOD, MOV, POS, NEG, ADDR, DEREF, JMP, JIF, JNT, 
	JLT, JGT, JEQ, JNE, JLE, JGE, PAR, CAL, RET, INDR, INDW, PTRW} opcode;
	Addr rs, rt, rd;
} Quad;

extern int quads_size, quads_capacity;
extern Quad *quads;

#define AImm(x) ((Addr){IMMEDIATE, .imm = x})
#define ASym(x) ((Addr){SYMBOL_A, .sym = x})

void Emit(Quad q);

#define Mov(dest, source) ((Quad){MOV, source, AImm(0), dest})
#define BinOp(op, dest, op1, op2) ((Quad){op, op1, op2, dest})
#define UnaryOp(op, dest, opr) ((Quad){op, opr, AImm(0), dest})
#define Jump(dest) ((Quad){JMP, AImm(0), AImm(0), dest})
#define JumpIf(dest, cond) ((Quad){JIF, cond, AImm(0), dest})
#define Param(source) ((Quad){PAR, source, AImm(0), AImm(0)})
#define Call(func) ((Quad){CAL, AImm(0), AImm(0), func})
#define CallAss(dest, func) ((Quad){CAL, dest, AImm(0), func})
#define Return() ((Quad){RET, AImm(0), AImm(0), AImm(0)})
#define IndexRead(dest, source, index) ((Quad){INDR, source, index, dest})
#define IndexWrite(dest, index, source) ((Quad){INDW, source, index, dest})
#define DerefWrite(dest, source) ((Quad){PTRW, dest, AImm(0), source})

void DisplayQuad(Quad q);
void DisplayQuads();

typedef struct _SymbolTable {
	const char *name;
	enum Scope {GLOBAL, FUNC, BLOCK} scope;
	struct _SymbolTable *parent;
	struct _Symbol *sym_head, *sym_tail;
	int temp_count;
} SymbolTable;

extern SymbolTable glb_table, *current_table;

SymbolTable *Create_SymbolTable(const char *name, enum Scope scope, SymbolTable *parent);
void Destroy_SymbolTable(SymbolTable *table);
void SymTableDispl(SymbolTable *table);

typedef enum {CHAR_T, INT_T} PRIMITIVE_TYPE;

typedef struct _Type {
	// needs some kind of types table + hashing mechanism to reuse types, instead of always creating new ones (wont implement for now)
	enum KIND_T {PRIMITIVE_PTR, TEMP_T, PRIMITIVE_T, ARRAY_T, FUNC_T} kind;
	union {
		PRIMITIVE_TYPE primitive;
		struct {
			PRIMITIVE_TYPE base;
			int size;
		} array;
		struct {
			struct _Type *return_type;
			struct _Type *param_list;
		} func;
	};
	struct _Type *next;
} Type;

int GetSize(Type type);
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
Symbol *SymLookupOrInsert(const char *name);
Symbol *GenTemp();
void SymInsert(Symbol *sym);
void SymFree(Symbol *sym);
void SymDispl(Symbol *sym);

#endif