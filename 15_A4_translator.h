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

// Used for true, false, and next lists within symbols
typedef struct _QuadList {
	int quad_index;
	struct _QuadList *next;
} QuadList;

extern int existing_lists_size, existing_lists_capacity;
extern QuadList **existing_lists;

void InitLists();
void AddList(QuadList *list);
void DestroyLists();

QuadList *MakeList(int ind);
void Insert(QuadList* list, int ind);
QuadList *Merge(QuadList *list1, QuadList *list2);
void Backpatch(QuadList *list, int dest);
void FreeList(QuadList *list);

typedef struct {
	enum OPCODE {ADD, SUB, MUL, DIV, MOD, MOV, POS, NEG, ADDR, DEREF, JMP, JIF, JNT, 
	JLT, JGT, JEQ, JNE, JLE, JGE, PAR, CAL, RET, INDR, INDW, PTRW, FN_LABEL} opcode;
	Addr rs, rt, rd;
} Quad;

extern int quads_size, quads_capacity;
extern Quad *quads;

#define AImm(x) ((Addr){IMMEDIATE, .imm = x})
#define ASym(x) ((Addr){SYMBOL_A, .sym = x.sym})

void Emit(Quad q);

#define Mov(dest, source) ((Quad){MOV, source, AImm(0), dest})
#define BinOp(op, dest, op1, op2) ((Quad){op, op1, op2, dest})
#define UnaryOp(op, dest, opr) ((Quad){op, opr, AImm(0), dest})
#define Jump(dest) ((Quad){JMP, AImm(0), AImm(0), dest})
#define JumpIf(dest, cond) ((Quad){JIF, cond, AImm(0), dest})
#define JumpIfNot(dest, cond) ((Quad){JNT, cond, AImm(0), dest})
#define JumpIfLess(dest, op1, op2) ((Quad){JLT, op1, op2, dest})
#define JumpIfGreater(dest, op1, op2) ((Quad){JGT, op1, op2, dest})
#define JumpIfEqual(dest, op1, op2) ((Quad){JEQ, op1, op2, dest})
#define JumpIfNotEqual(dest, op1, op2) ((Quad){JNE, op1, op2, dest})
#define JumpIfLessEqual(dest, op1, op2) ((Quad){JLE, op1, op2, dest})
#define JumpIfGreaterEqual(dest, op1, op2) ((Quad){JGE, op1, op2, dest})
#define Param(source) ((Quad){PAR, source, AImm(0), AImm(0)})
#define Call(func, args) ((Quad){CAL, AImm(0), args, func})
#define CallAss(dest, func, args) ((Quad){CAL, dest, args, func})
#define Return(val) ((Quad){RET, val, AImm(0), AImm(0)})
#define IndexRead(dest, source, index) ((Quad){INDR, source, index, dest})
#define IndexWrite(dest, index, source) ((Quad){INDW, source, index, dest})
#define DerefWrite(dest, source) ((Quad){PTRW, dest, AImm(0), source})
#define FnLabel(label) ((Quad){FN_LABEL, AImm(0), AImm(0), label})

void DisplayQuad(Quad q);
void DisplayQuads();

typedef struct _SymbolTable {
	const char *name;
	enum Scope {GLOBAL, FUNC, BLOCK} scope;
	struct _SymbolTable *parent;
	struct _Symbol *sym_head, *sym_tail;
	int temp_count;
	int offset;
} SymbolTable;

extern SymbolTable glb_table, *current_table;

SymbolTable *Create_SymbolTable(const char *name, enum Scope scope, SymbolTable *parent);
void Destroy_SymbolTable(SymbolTable *table);
void SymTableDispl(SymbolTable *table);
typedef enum { INT_T, CHAR_T, VOID_T } PRIMITIVE_TYPE;

#define prim2type(x) ((Type){.kind = PRIMITIVE_T, .primitive = x})

struct _ArgList;

typedef struct _Type {
	// needs some kind of types table + hashing mechanism to reuse types, instead of always creating new ones (wont implement for now)
	enum KIND_T {PRIMITIVE_PTR, ARRAY_PTR, PRIMITIVE_T, ARRAY_T, FUNC_T} kind;
	union {
		PRIMITIVE_TYPE primitive;
		struct {
			PRIMITIVE_TYPE base;
			int size;
		} array;
		struct {
			struct _Type *return_type;
			struct _ArgList *arg_list;
		} func;
	};
} Type;

int GetSize(Type type);
char TypeEq(Type t1, Type t2);
char TypeCompatible(Type t1, Type t2);
void TypeFree(Type *type);
void TypeDispl(Type type);

typedef struct _Symbol {
	const char *name;
	Type type;
	int initial_value;
	int size;
	int offset;
	char is_temp;
	SymbolTable *inner_table;
	struct _Symbol *next;
} Symbol;

Symbol *SymInit(enum KIND_T kind);
Symbol *SymClone(Symbol *sym);
Symbol *Cast(Symbol *sym, Type type);
Symbol *SymLookup(const char *name);
Symbol *SymLookupOrInsert(const char *name);
Symbol *StringLookupOrInsert(const char *str);
Symbol *GenTemp();
void SymInsert(Symbol *sym);
void SymFree(Symbol *sym);
void SymDispl(Symbol *sym);

typedef struct _ExprAttrib {
	Symbol *sym;
	QuadList *truelist, *falselist;
} ExprAttrib;

#define PURE_EXPR(x) ((ExprAttrib){.sym = x, .truelist = NULL, .falselist = NULL})
#define BOOL_EXPR(x, tl, fl) ((ExprAttrib){.sym = x, .truelist = tl, .falselist = fl})

typedef struct _ArgListElem {
	enum {EXPR, DECL} kind;
	union {
		ExprAttrib expr;
		struct {
			Type type;
			const char *name;
		} decl;
	};
} ArgListElem;

typedef struct _ArgList {
	ArgListElem elem;
	struct _ArgList *next;
} ArgList;

#define ARG_EXPR(x) ((ArgListElem){.kind = EXPR, .expr = x})
#define ARG_DECL(t, n) ((ArgListElem){.kind = DECL, .decl = {t, n}})

ArgList *MakeArgList(ArgListElem elem);
void InsertArg(ArgList *list, ArgListElem elem);
void DestroyArgList(ArgList *list);

#endif