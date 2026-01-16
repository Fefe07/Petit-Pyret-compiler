	.text
	.globl	main
main:
	movq %rsp, %rbp
	movq $16, %rdi
	call my_malloc
	movq $2, %rdi
	movq %rdi, 0(%rax)
	movq $42, %rdi
	movq %rdi, 8(%rax)
	pushq 8(%rax)
	movq $16, %rdi
	call my_malloc
	movq $2, %rdi
	movq %rdi, 0(%rax)
	movq $69, %rdi
	movq %rdi, 8(%rax)
	movq 8(%rax), %rdx
	popq %rax
	cmpq %rdx, %rax
	jge l21
	movq $1, %rax
	jmp l22
l21:
	movq $0, %rax
l22:
	cmpq $0, %rax
	jne 1f
	movq $16, %rdi
	call my_malloc
	movq $2, %rdi
	movq %rdi, 0(%rax)
	movq $0, %rdi
	movq %rdi, 8(%rax)
	movq 8(%rax), %rsi
	pushq %rax
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	popq %rax
	jmp 2f
1:
	movq $16, %rdi
	call my_malloc
	movq $2, %rdi
	movq %rdi, 0(%rax)
	movq $1, %rdi
	movq %rdi, 8(%rax)
	movq 8(%rax), %rsi
	pushq %rax
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	popq %rax
2:
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
