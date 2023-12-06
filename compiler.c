#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "compiler.h"

const char *CONST_DIRECTIVES[] = {
	[1] = ".byte",
	[2] = ".short",
	[4] = ".long",
	[8] = ".quad"
};

static void WriteConstSym(Symbol *sym, FILE *file) {
	static int strings_count = 0;

	if (!sym->is_temp && sym->type.kind != ARRAY_T) {
		fprintf(file, ".global %s\n", sym->name);
	}

	switch (sym->type.kind) {
		case PRIMITIVE_T:
		case PRIMITIVE_PTR:
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
			WriteConstSym(sym, file);
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

static void TranslateQuad(Quad q, FILE *file, SymbolTable *tab) {

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
			fprintf(file, "\t");
			TranslateQuad(quads[j], file, &glb_table);
			fprintf(file, "\n");
		}
	}

	fprintf(file, "\n");

	// jump to nanoC file's entry point, main, now called _main
	fprintf(file, "\tcall _main\n");
}