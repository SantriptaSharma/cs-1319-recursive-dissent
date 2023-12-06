.data
STRING1:
	.string "Hello, world!\n"
.text
	.globl _main
	.globl printStr

_main:
	# write string1 to stdout
	pushl $STRING1
	call printStr
	addl $4, %esp

	# exit w code 0
	xorl %edi, %edi
	movl $0x3c, %eax
	syscall

printStr:
	# prologue, no callee-saved registers
	pushl %ebp
	movl %esp, %ebp 

	# let's get the (32-bit) pointer out of the stack
	movl 8(%ebp), %ecx
	# edx will calc strlen
	xorl %edx, %edx
	_lenloop:
		cmpb $0, (%ecx, %edx, 1)
		je _lenloop_end
		incl %edx
		jmp _lenloop
	_lenloop_end:
	# edx now contains strlen
	# need to write to stdout, fd = 1
	movl $1, %ebx
	# syscall number for write is 4 on 32-bit
	movl $4, %eax

	int $4
	# eax contains return value from write, no. of written characters
	# our calling convention dictates eax as the return value, so all
	# done

	# epilogue
	movl %esp, %ebp
	popl %ebp
	ret
