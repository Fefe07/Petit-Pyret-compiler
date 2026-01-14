	.text
	.globl	main
main:
	movq %rsp, %rbp
	movq $16, %rdi
	call my_malloc
	movq $2, %rdi
	movq %rdi, 0(%rax)
	movq $122, %rdi
	movq %rdi, 8(%rax)
	movq 8(%rax), %rsi
	pushq %rax
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	popq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, %rdi
	movq %rdi, 0(%rax)
	movq $486, %rdi
	movq %rdi, 8(%rax)
	pushq 8(%rax)
	movq $16, %rdi
	call my_malloc
	movq $2, %rdi
	movq %rdi, 0(%rax)
	movq $4, %rdi
	movq %rdi, 8(%rax)
	movq 8(%rax), %rdx
	popq %rax
	addq %rdx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, %rdi
	movq %rdi, 0(%rax)
	popq %rdi
	movq %rdi, 8(%rax)
	movq 8(%rax), %rsi
	pushq %rax
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	popq %rax
	movq $0, %rax
	ret
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call malloc
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
.Sprint_int:
	.string "%d\n"
