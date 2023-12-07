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
	mov $0x4, %rax
	mov $1, %rbx
	mov 8(%rsp), %rcx
	# TODO:dynamically calc length
	mov $14, %rdx
	int $0x80
	ret

printInt:
	ret

readInt:
	ret