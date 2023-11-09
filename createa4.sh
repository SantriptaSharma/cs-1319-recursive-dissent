cp tex_build/15_A4.pdf .
tar -c 15_A4_translator.c 15_A4_translator.h 15_A4.l 15_A4.nc 15_A4.y Makefile 15_A4.pdf -f 15_A4.tar
rm 15_A4.pdf