.data
.global _main
.text
.global main
main:

	call _main
	exit_loop:
	movl %eax, %ebx
	movl $0x1, %eax
	int $0x80
	jmp exit_loop

_main:
#	function prologue
	push %rbp
	mov %rsp, %rbp

#	local __t_0_ (-8 =?= -8)
	sub $8, %rsp
	movq $5, (%rsp)

#	local a (-12 =?= -12)
	sub $4, %rsp
	movl $0, (%rsp)

#	local num (-20 =?= -20)
	sub $8, %rsp
	movq $0, (%rsp)

#	local __t_1_ (-28 =?= -28)
	sub $8, %rsp
	movq $10, (%rsp)

#	local num2 (-36 =?= -36)
	sub $8, %rsp
	movq $0, (%rsp)

#	local __t_2_ (-44 =?= -44)
	sub $8, %rsp
	movq $1, (%rsp)

#	local __t_3_ (-52 =?= -52)
	sub $8, %rsp
	movq $0, (%rsp)

#	local __t_4_ (-60 =?= -60)
	sub $8, %rsp
	movq $0, (%rsp)

#	local __t_5_ (-68 =?= -68)
	sub $8, %rsp
	movq $5, (%rsp)

#	local __t_6_ (-76 =?= -76)
	sub $8, %rsp
	movq $-5, (%rsp)

#	local __t_7_ (-84 =?= -84)
	sub $8, %rsp
	movq $1, (%rsp)

#	local __t_8_ (-92 =?= -92)
	sub $8, %rsp
	movq $0, (%rsp)

#	local __t_9_ (-100 =?= -100)
	sub $8, %rsp
	movq $5, (%rsp)

#	local __t_10_ (-108 =?= -108)
	sub $8, %rsp
	movq $1, (%rsp)

#	local __t_11_ (-116 =?= -116)
	sub $8, %rsp
	movq $5, (%rsp)

#	local __t_12_ (-124 =?= -124)
	sub $8, %rsp
	movq $1, (%rsp)

#	local b (-128 =?= -128)
	sub $4, %rsp
	movl $0, (%rsp)

#	local __t_13_ (-136 =?= -136)
	sub $8, %rsp
	movq $0, (%rsp)

#	local deref__num (-140 =?= -140)
	sub $4, %rsp
	movl $0, (%rsp)

#	local __t_14_ (-148 =?= -148)
	sub $8, %rsp
	movq $5, (%rsp)

#	local __t_15_ (-156 =?= -156)
	sub $8, %rsp
	movq $1, (%rsp)

#	local __t_16_ (-164 =?= -164)
	sub $8, %rsp
	movq $5, (%rsp)

#	local __t_17_ (-172 =?= -172)
	sub $8, %rsp
	movq $1, (%rsp)

#	local __t_18_ (-180 =?= -180)
	sub $8, %rsp
	movq $10, (%rsp)

#	local __t_19_ (-188 =?= -188)
	sub $8, %rsp
	movq $10, (%rsp)

#	local __t_20_ (-196 =?= -196)
	sub $8, %rsp
	movq $0, (%rsp)

#	local __t_21_ (-204 =?= -204)
	sub $8, %rsp
	movq $1, (%rsp)

#	local __t_22_ (-212 =?= -212)
	sub $8, %rsp
	movq $0, (%rsp)


#	function body

#	a = __t_0_(5)
	movl $5, %ebx
	movl %ebx, -12(%rbp)

#	_L_0_:
	_L_0_:

#	_L_1_:
	_L_1_:

#	num2 = __t_1_(10)
	movq $10, %rbx
	movq %rbx, -36(%rbp)

#	_L_2_:
	_L_2_:

#	__t_3_ = num2 + __t_2_(1)
	movq -36(%rbp), %rax
	movq $1, %rbx
	addq %rbx, %rax
	movq %rax, -52(%rbp)

#	num2 = __t_3_
	movq -52(%rbp), %rbx
	movq %rbx, -36(%rbp)

#	_L_3_:
	_L_3_:

#	__t_4_ = -a
	movsxd -12(%rbp), %rax
	negq %rax
	movq %rax, -60(%rbp)

#	a = __t_4_
	movl -60(%rbp), %ebx
	movl %ebx, -12(%rbp)

#	_L_4_:
	_L_4_:

#	if a != __t_6_(-5) goto 14
	movl -12(%rbp), %ebx
	cmp $-5, %ebx
	jne _L_5_

#	goto 18
	jmp _L_7_

#	_L_5_:
	_L_5_:

#	_L_6_:
	_L_6_:

#	__retval = __t_7_(1)
	movl $1, %eax

#	return
	jmp _f__main_return_

#	_L_7_:
	_L_7_:

#	__t_8_ = -a
	movsxd -12(%rbp), %rax
	negq %rax
	movq %rax, -92(%rbp)

#	a = __t_8_
	movl -92(%rbp), %ebx
	movl %ebx, -12(%rbp)

#	_L_8_:
	_L_8_:

#	if a != __t_9_(5) goto 24
	movl -12(%rbp), %ebx
	cmp $5, %ebx
	jne _L_9_

#	goto 28
	jmp _L_11_

#	_L_9_:
	_L_9_:

#	_L_10_:
	_L_10_:

#	__retval = __t_10_(1)
	movl $1, %eax

#	return
	jmp _f__main_return_

#	_L_11_:
	_L_11_:

#	a = a
	movl -12(%rbp), %ebx
	movl %ebx, -12(%rbp)

#	_L_12_:
	_L_12_:

#	if a != __t_11_(5) goto 33
	movl -12(%rbp), %ebx
	cmp $5, %ebx
	jne _L_13_

#	goto 37
	jmp _L_15_

#	_L_13_:
	_L_13_:

#	_L_14_:
	_L_14_:

#	__retval = __t_12_(1)
	movl $1, %eax

#	return
	jmp _f__main_return_

#	_L_15_:
	_L_15_:

#	b = num
	movl -20(%rbp), %ebx
	movl %ebx, -128(%rbp)

#	_L_16_:
	_L_16_:

#	__t_13_ = &a
	leaq -12(%rbp), %rax
	movq %rax, -136(%rbp)

#	num = __t_13_
	movq -136(%rbp), %rbx
	movq %rbx, -20(%rbp)

#	_L_17_:
	_L_17_:

#	deref__num = *num
	movq -20(%rbp), %rax
	movq (%rax), %rax
	movq %rax, -140(%rbp)

#	a = deref__num
	movl -140(%rbp), %ebx
	movl %ebx, -12(%rbp)

#	_L_18_:
	_L_18_:

#	if a != __t_14_(5) goto 48
	movl -12(%rbp), %ebx
	cmp $5, %ebx
	jne _L_19_

#	goto 52
	jmp _L_21_

#	_L_19_:
	_L_19_:

#	_L_20_:
	_L_20_:

#	__retval = __t_15_(1)
	movl $1, %eax

#	return
	jmp _f__main_return_

#	_L_21_:
	_L_21_:

#	deref__num = *num
	movq -20(%rbp), %rax
	movq (%rax), %rax
	movq %rax, -140(%rbp)

#	if deref__num != __t_16_(5) goto 56
	movl -140(%rbp), %ebx
	cmp $5, %ebx
	jne _L_22_

#	goto 60
	jmp _L_24_

#	_L_22_:
	_L_22_:

#	_L_23_:
	_L_23_:

#	__retval = __t_17_(1)
	movl $1, %eax

#	return
	jmp _f__main_return_

#	_L_24_:
	_L_24_:

#	deref__num = *num
	movq -20(%rbp), %rax
	movq (%rax), %rax
	movq %rax, -140(%rbp)

#	deref__num = __t_18_(10)
	movq -20(%rbp), %rax
	movq $10, %rbx
	movl %ebx, (%rax)

#	_L_25_:
	_L_25_:

#	if a == __t_19_(10) goto 66
	movl -12(%rbp), %ebx
	cmp $10, %ebx
	je _L_26_

#	goto 68
	jmp _L_27_

#	_L_26_:
	_L_26_:

#	goto 70
	jmp _L_28_

#	_L_27_:
	_L_27_:

#	goto 73
	jmp _L_29_

#	_L_28_:
	_L_28_:

#	__t_22_ = __t_20_
	movq -196(%rbp), %rbx
	movq %rbx, -212(%rbp)

#	goto 75
	jmp _L_30_

#	_L_29_:
	_L_29_:

#	__t_22_ = __t_21_(1)
	movq $1, %rbx
	movq %rbx, -212(%rbp)

#	_L_30_:
	_L_30_:

#	_L_31_:
	_L_31_:

#	__retval = __t_22_
	movl -212(%rbp), %eax

#	return
	jmp _f__main_return_

#	function epilogue
	_f__main_return_:
	mov %rbp, %rsp
	pop %rbp
	ret


