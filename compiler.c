#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "compiler.h"

#define MAX(a, b) ((a) > (b) ? (a) : (b))

const char *CONST_DIRECTIVES[] = {
	[1] = ".byte",
	[2] = ".short",
	[4] = ".long",
	[8] = ".quad"
};

static void WriteGlobalSym(Symbol *sym, FILE *file) {
	static int strings_count = 0;

	if (!sym->is_temp && sym->type.kind != ARRAY_T) {
		fprintf(file, ".global %s\n", sym->name);
	}

	switch (sym->type.kind) {
		case PRIMITIVE_T:
		case PRIMITIVE_PTR:
			if (sym->initial_value != 0) { break; } // will be a literal in the assembly code
			fprintf(file, "%s:\n\t%s %d\n", sym->name, CONST_DIRECTIVES[sym->size], sym->initial_value);
		break;

		case ARRAY_T:
			if (sym->type.array.base == CHAR_T) {
				size_t it = 0;
				const char *pref = "__str_";

				while (sym->name[it] == pref[it]) {
					it++;
				}

				if (pref[it] == '\0') {
					sym->initial_value = strings_count++;
					fprintf(file, "_user_string_%d:\n\t.string \"%s\"\n", sym->initial_value, sym->name + it);
				} else {
					fprintf(file, ".global %s\n", sym->name);
					fprintf(file, "%s:\n\t.zero %d\n", sym->name, sym->size);
				}
			} else {
				fprintf(file, ".global %s\n", sym->name);
				fprintf(file, "%s:\n\t.zero %d\n", sym->name, sym->size);
			}
		break;

		case ARRAY_PTR:
			fprintf(file, "%s:\n\t.zero %d\n", sym->name, sym->size);
		break;
	}
}

void WriteDataSeg(FILE *file) {	
	fprintf(file, ".data\n");
	
	Symbol *sym = glb_table.sym_head;
	
	while (sym != NULL) {
		if (sym->type.kind != FUNC_T) {
			WriteGlobalSym(sym, file);
		} else {
			if (strcmp(sym->name, "main") == 0) {
				char *main_label = malloc(strlen(sym->name) + 2);
				sprintf(main_label, "_main", sym->name);
				free((void *)sym->name);
				sym->name = main_label;
			}
			fprintf(file, ".global %s\n", sym->name);
		}
		sym = sym->next;
	}
}

static struct QuadRange {
	int start, end;
} ext_quad_blocks[512], func_quad_blocks[512];

static int ext_count = 0, func_count = 0;

static void FindQuadExtents() {
	int next = 0;
	
	// the start position of the last external quad block
	int start = 0;

	int i = 0;

	while (i < quads_size) {
		Quad q = quads[i];

		if (q.opcode == FN_LABEL && q.rd.kind == SYMBOL_A) {
			if (i != start) {
				ext_quad_blocks[next].start = start;
				ext_quad_blocks[next].end = i - 1;
				next++;
			}

			// find last return statement in the function, scan until eof or next function label
			int lastret = i;
			int j;
			for (j = i + 1; j < quads_size; j++) {
				Quad q2 = quads[j];

				if (q2.opcode == FN_LABEL && q2.rd.kind == SYMBOL_A) {
					break;
				} 

				if (q2.opcode == RET) {
					lastret = j;
				}
			}

			if (j == quads_size && lastret == i) lastret = quads_size - 1;

			func_quad_blocks[func_count].start = i;
			func_quad_blocks[func_count].end = lastret;
			func_count++;

			i = lastret;
			start = i + 1;
		}

		i++;
	}

	if (i != start) {
		ext_quad_blocks[next].start = start;
		ext_quad_blocks[next].end = i - 1;
		next++;
	}

	ext_count = next;
}

static const char postfixes[] = {
	[4] = 'l', [8] = 'q'
};

// redundant rn
static const char movpostfix[] = {
	[4] = 'l', [8] = 'q'
};

static const char regprefixes[] = {
	[4] = 'e', [8] = 'r'
};

static const char *arithmetic_ops[] ={
	[ADD] = "add",
	[SUB] = "sub",
	[MUL] = "imul",
	[DIV] = "idiv",
	[MOD] = "idiv"
};

// holds context for the current function being translated
struct {
	int argc;

	// args above retval, retval never gets put into memory (alias for %rax)
	int retvaloff;
	
	// after retval, first local var's offset
	int localsoff;

	// size of the stack frame, only locals included
	int stacksize;

	// function symbol
	Symbol *sym;
} func_context;

static void TranslateOperand(Addr a, FILE *file) {
	switch (a.kind) {
		case SYMBOL_A:
			// insert constants as they are
			if (a.sym->initial_value != 0) {
				fprintf(file, "$%d", a.sym->initial_value);
				return;
			}
			
			// if the symbol is a string literal
			if (a.sym->type.kind == ARRAY_T && a.sym->type.array.base == CHAR_T) {
				size_t it = 0;
				const char *pref = "__str_";

				while (a.sym->name[it] == pref[it]) {
					it++;
				}

				if (pref[it] == '\0') {
					fprintf(file, "$_user_string_%d", a.sym->initial_value);
					return;
				}
			}

			// if we are not in a function, this is just a global var
			if(current_table == &glb_table) {
				if (a.sym->type.kind == ARRAY_T || a.sym->type.kind == ARRAY_PTR) {
					// the value of an array is its address
					fprintf(file, "$%s", a.sym->name);
				} else {
					// return the variable
					fprintf(file, "%s", a.sym->name);
				}
				return;
			}

			// check if symbol in glb table, if so, use variable
			Symbol *sym = SymLookup(a.sym->name, 0);

			if (sym == NULL) {
				// must be in global table, only one level of nesting allowed
				if (a.sym->type.kind == ARRAY_T || a.sym->type.kind == ARRAY_PTR) {
					// the value of an array is its address
					fprintf(file, "$%s", a.sym->name);
				} else {
					// return the literal value of the given variable
					fprintf(file, "%s", a.sym->name);
				}
				return;
			}

			// otherwise, check if symbol is a parameter, if so, use +offset(%rbp)
			// use func_context to figure this out
			if (sym->offset < func_context.retvaloff) {
				fprintf(file, "%d(%%rbp)", sym->offset - func_context.retvaloff - size_of_pointer);
				return;
			}

			// otherwise, use -offset(%rbp)
			fprintf(file, "-%d(%%rbp)", sym->offset - func_context.localsoff);
		break;

		case IMMEDIATE:
			fprintf(file, "$%d", a.imm);
		break;
	}
}

static void LoadBinOps(Quad q, FILE *file) {
	size_t sd = q.rd.sym->size;
	size_t ss = q.rs.sym->size;
	size_t st = q.rt.sym->size;

	// for sign extension char -> 32/64 requires movsx, int -> 64 requires movsxd
	const char *extension_instr = q.rs.sym->initial_value == 0 ? 
	(ss == 1 ? "movsx" : "movsxd") 
	: "mov";
	// sorry, but if the symbol is a constant, we can't use movsx/movsxd

	if (q.opcode == DIV || q.opcode == MOD) {
		// zero out rdx, upper 64 bits of the dividend
		fprintf(file, "xor %%rdx, %%rdx\n\t");
		fprintf(file, "%s ", extension_instr);
		TranslateOperand(q.rs, file);
		fprintf(file, ", %%rax\n\t");

		extension_instr = q.rt.sym->initial_value == 0 ? 
		(st == 1 ? "movsx" : "movsxd") 
		: "mov";

		fprintf(file, "%s ", extension_instr);
		TranslateOperand(q.rt, file);
		fprintf(file, ", %%%cbx\n\t", regprefixes[st]);
		return;	
	}

	fprintf(file, "mov%c ", movpostfix[ss]);
	TranslateOperand(q.rs, file);
	fprintf(file, ", %%%cax\n\t", regprefixes[ss]);

	// sign extension

	if (ss < sd && q.rs.sym->initial_value == 0) {
		fprintf(file, "%s %%%cax, %%%cax\n\t", extension_instr, regprefixes[ss], regprefixes[sd]);
	}

	fprintf(file, "mov%c ", movpostfix[st]);
	TranslateOperand(q.rt, file);
	fprintf(file, ", %%%cbx\n\t", regprefixes[st]);

	if (st < sd && q.rt.sym->initial_value == 0) {
		extension_instr = st == 1 ? "movsx" : "movsxd";

		fprintf(file, "%s %%%cbx, %%%cbx\n\t", extension_instr, regprefixes[st], regprefixes[sd]);
	}
}

static void TranslateQuad(Quad q, FILE *file, SymbolTable *tab) {
	SymbolTable *saved = current_table;
	current_table = tab;

	switch (q.opcode) {
		size_t sd, ss, st;
		
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case MOD:
			sd = q.rd.sym->size;
			ss = q.rs.sym->size;
			st = q.rt.sym->size;
			
			// first we get the operands in rbx/ebx and rax/eax respectively (except for division, where it's flipped)
			LoadBinOps(q, file);
			// size for the operation
			int size = MAX(ss, st);

			// now we perform the operation

			if (q.opcode == DIV || q.opcode == MOD) {
				fprintf(file, "idiv %%%cbx\n\t", regprefixes[st]);

				if (q.opcode == DIV) {
					fprintf(file, "mov%c %%rax, ", movpostfix[sd]);
					TranslateOperand(q.rd, file);
					fprintf(file, "\n\t");
				} else {
					fprintf(file, "mov%c %%rdx, ", movpostfix[sd]);
					TranslateOperand(q.rd, file);
					fprintf(file, "\n\t");
				}
				break;
			}

			fprintf(file, "%s%c %%%cbx, %%%cax\n\t", arithmetic_ops[q.opcode], postfixes[size], regprefixes[sd], regprefixes[sd]);

			// now we store the result
			fprintf(file, "mov%c %%%cax, ", movpostfix[sd], regprefixes[sd]);
			TranslateOperand(q.rd, file);
		break;

		case ADDR:
		case DEREF:
			// TODO:
		break;

		case NEG:
		case POS:
			// TODO:
			fprintf(file, "%s%c ", q.opcode == NEG ? "neg" : "not", postfixes[q.rd.sym->size]);
			TranslateOperand(q.rs, file);
		break;

		case JMP:
		break;

		case JIF:
		case JNT:
		break;

		case JLT:
		case JLE:
		break;

		case JGT:
		case JGE:
		break;

		case JEQ:
		case JNE:
		break;

		case MOV:
			sd = q.rd.sym->size;
			ss = q.rs.sym->size;

			if (strcmp(q.rd.sym->name, "__retval") == 0) {
				if (func_context.sym->type.func.return_type->kind == PRIMITIVE_T && func_context.sym->type.func.return_type->primitive == VOID_T) {
					// do nothing
					break;
				}

				fprintf(file, "mov%c ", movpostfix[sd]);
				TranslateOperand(q.rs, file);
				fprintf(file, ", %%%cax", regprefixes[sd]);
				break;
			}

			fprintf(file, "mov%c ", movpostfix[sd]);
			TranslateOperand(q.rs, file);
			fprintf(file, ", %%%cbx\n\t", regprefixes[sd]);
			fprintf(file, "mov%c %%%cbx, ", movpostfix[sd], regprefixes[sd]);
			TranslateOperand(q.rd, file);
		break;

		case INDR:
		case INDW:
			// TODO:
		break;

		case PTRW:
			// TODO:
		break;
	
		case FN_LABEL:
			if (q.rd.kind == SYMBOL_A) 
				fprintf(file, "%s:", q.rd.sym->name);
			else
				fprintf(file, "_L_%d_:", q.rd.imm);
		break;

		case RET:
			fprintf(file, "jmp _f_%s_return_", func_context.sym->name);
		break;

		case PAR:
			// TODO:
		break;

		case CAL:
			fprintf(file, "call %s", q.rd.sym->name);
		break;
	}

	current_table = saved;
}

void WriteEntryPoint(FILE *file) {
	FindQuadExtents();

	for (int i = 0; i < ext_count; i++) {
		printf("External quad block %d: %d - %d\n", i, ext_quad_blocks[i].start, ext_quad_blocks[i].end);
	}

	for (int i = 0; i < func_count; i++) {
		printf("Function quad block %s: %d - %d\n", quads[func_quad_blocks[i].start].rd.sym->name, func_quad_blocks[i].start, func_quad_blocks[i].end);
	}

	// write the entry point
	fprintf(file, ".text\n");
	fprintf(file, ".global main\n");
	fprintf(file, "main:\n");
	// first, write the external quad blocks
	for (int i = 0; i < ext_count; i++) {
		for (int j = ext_quad_blocks[i].start; j <= ext_quad_blocks[i].end; j++) {
			fprintf(file, "#\t");
			DisplayQuad(quads[j], file);
			fprintf(file, "\n\t");
			TranslateQuad(quads[j], file, &glb_table);
			fprintf(file, "\n\n");
		}
	}

	fprintf(file, "\n");

	// jump to nanoC file's entry point, main, now called _main
	fprintf(file, "\tcall _main\n");

	// when _main returns, exit w exit code returned by main (in eax)
	fprintf(file, "\texit_loop:\n");
	fprintf(file, "\tmovl %%eax, %%ebx\n");
	fprintf(file, "\tmovl $0x1, %%eax\n");
	fprintf(file, "\tint $0x80\n");
	fprintf(file, "\tjmp exit_loop\n\n");
}

void WriteFunction(FILE *file, int i) {
	for (int j = func_quad_blocks[i].start + 1; j <= func_quad_blocks[i].end; j++) {
		fprintf(file, "#\t");
		DisplayQuad(quads[j], file);
		fprintf(file, "\n\t");
		TranslateQuad(quads[j], file, func_context.sym->inner_table);
		fprintf(file, "\n\n");
	}
}

void WriteFunctions(FILE *file) {
	for (int i = 0; i < func_count; i++) {
		// write the function label
		Quad q = quads[func_quad_blocks[i].start];
		fprintf(file, "%s:\n", q.rd.sym->name);

		// reset context
		func_context.argc = 0;
		func_context.retvaloff = 0;
		func_context.localsoff = 0;
		func_context.stacksize = 0;
		func_context.sym = q.rd.sym;

		Symbol *sym = q.rd.sym->inner_table->sym_head;
		while (sym != NULL) {
			if (strcmp(sym->name, "__retval") == 0) {
				func_context.retvaloff = sym->offset;
				func_context.localsoff = sym->offset + sym->size;
				sym = sym->next;
				break;
			}

			func_context.argc++;
			sym = sym->next;
		}

		if (sym != NULL && sym->offset != func_context.localsoff) {
			printf("Aborting: symbol table invalid somehow\n");
			return;
		}

		// write the function prologue
		fprintf(file, "#\tfunction prologue\n\t");
		fprintf(file, "push %%rbp\n\t");
		fprintf(file, "mov %%rsp, %%rbp\n\n");
		
		while (sym != NULL) {
			fprintf(file, "#\tlocal %s (-%d = -%d)\n\t", sym->name, func_context.stacksize, sym->offset - func_context.localsoff);
			fprintf(file, "movq	%%rsp, %%rax\n\t");
			fprintf(file, "sub $%d, %%rsp\n\t", sym->size);
			
			if (sym->type.kind == ARRAY_T || sym->type.kind == ARRAY_PTR) {
				// fill the array w zeros
				fprintf(file, "mov $%d, %%rcx\n\t", sym->size);
				fprintf(file, "mov %%rax, %%rdi\n\t");
				fprintf(file, "xor %%eax, %%eax\n\t");
				fprintf(file, "rep stosb\n\n");
			} else {
				fprintf(file, "mov%c $%d, (%%rax)\n\n", movpostfix[sym->size], sym->initial_value);
			}

			func_context.stacksize += sym->size;
			sym = sym->next;
		}

		fprintf(file, "\n#\tfunction body\n\n");
		WriteFunction(file, i);
		
		// write the function epilogue
		fprintf(file, "#\tfunction epilogue\n\t");
		fprintf(file, "_f_%s_return_:\n\t", func_context.sym->name);
		fprintf(file, "mov %%rbp, %%rsp\n\t");
		fprintf(file, "pop %%rbp\n\t");
		fprintf(file, "ret\n\n");
	}
}