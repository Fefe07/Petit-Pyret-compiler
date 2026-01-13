	.text
	.globl	main
main:
	movq %rsp, %rbp
	movq $1, %rax
	movq %rax, %rdx
	movq $4, %rax
	movq %rax, %rsi
	movq %rax, %r12
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	movq %r12, %rax
	subq %rdx, %rax
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
