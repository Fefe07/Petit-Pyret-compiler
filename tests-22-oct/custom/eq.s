	.text
	.globl	main
main:
	movq %rsp, %rbp
	movq $42, %rax
	pushq %rax
	movq $69, %rax
	movq %rax, %rdx
	popq %rax
	cmpq %rdx, %rax
	jne l21
	movq $1, %rax
	jmp l22
l21:
	movq $0, %rax
l22:
	movq %rax, %rsi
	movq %rax, %r12
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	movq %r12, %rax
	movq $0, %rax
	ret
	.data
.Sprint_int:
	.string "%d\n"
