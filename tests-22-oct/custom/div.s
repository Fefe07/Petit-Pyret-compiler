	.text
	.globl	main
main:
	movq %rsp, %rbp
	movq $42, %rax
	pushq %rax
	movq $43, %rax
	movq %rax, %rdx
	popq %rax
	movq %rdx, %rbx
	movq $0, %rdx
	idivq %rbx
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
