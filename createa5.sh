cp tex_build/15_A5.pdf .
tar -c 15_A5_translator.c 15_A5_translator.h compiler.c compiler.h 15_A5.l 15_A5.nc 15_A5.y Makefile 15_A5.pdf 15_A5_quads* -f 15_A5.tar
rm 15_A5.pdf