	.text
	.globl	main
main:
	movq %rsp, %rbp
	movq $4, %rax
	movq %rax, %rsi
	movq %rax, %r12
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	movq %r12, %rax
	pushq %rax
	movq $7, %rax
	pushq %rax
	movq $1, %rax
	movq %rax, %rsi
	movq %rax, %r12
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	movq %r12, %rax
	movq %rax, %rdx
	popq %rax
	addq %rdx, %rax
	movq %rax, %rsi
	movq %rax, %r12
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	movq %r12, %rax
	movq %rax, %rdx
	popq %rax
	subq %rdx, %rax
	movq %rax, %rsi
	movq %rax, %r12
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	movq %r12, %rax
	movq $122, %rax
	pushq %rax
	movq $3, %rax
	movq %rax, %rdx
	popq %rax
	imulq %rdx, %rax
	movq %rax, %rsi
	movq %rax, %r12
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	movq %r12, %rax
	movq $0, %rax
	ret
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	movq 24(%rbp), %rdi
	call malloc
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
.Sprint_int:
	.string "%d\n"
