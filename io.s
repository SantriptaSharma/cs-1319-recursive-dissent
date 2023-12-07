# for testing
.data
	hello: .string "hlo world!\n"

.text
# a simple I/O lib
.global printStr
.global printInt
.global readInt
.global main

main:
	push $hello
	call printStr
	add $8, %rsp
	# exit
	mov $0x1, %rax
	mov $0, %rbx
	int $0x80

printStr:
	lea 8(%rsp), %rdi
	mov $0, %rsi
	count_loop:
		cmpb $0, (%rdi)
		je count_end
		inc %rdi
		inc %rsi
		jmp count_loop
	count_end:

	mov $0x4, %rax
	mov $1, %rbx
	mov 8(%rsp), %rcx
	mov %rsi, %rdx
	int $0x80
	ret

printInt:
	ret

readInt:
	ret