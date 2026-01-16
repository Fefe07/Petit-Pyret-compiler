	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call my_malloc
	movq $6, 0(%rax)
	leaq num_modulo(%rip), %rdx
	movq %rdx, 8(%rax)
	pushq %rax
	movq $8, %rdi
	call my_malloc
	movq $0, 0(%rax)
	pushq %rax
	jmp l21
l22:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rdi
	movq 0(%rdi), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
l21:
	movq $24, %rdi
	call my_malloc
	movq $6, 0(%rax)
	leaq l22(%rip), %rdx
	movq %rdx, 8(%rax)
	pushq %rax
	movq -16(%rbp), %rax
	popq %rdi
	popq %rax
	movq %rdi, 16(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq -24(%rbp), %rax
	pushq %rax
	call *8(%rax)
	popq %rdi
	popq %rdi
	movq $0, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	call malloc
	movq %rbp, %rsp
	popq %rbp
	ret
print:
	pushq %rbp
	pushq %rdi
	movq %rsp, %rbp
	cmpq $0, 0(%rdi)
	je 0f
	cmpq $1, 0(%rdi)
	je 1f
	cmpq $2, 0(%rdi)
	je 2f
	cmpq $3, 0(%rdi)
	je 3f
	jmp 7f
0:
	movq $.nothing, %rdi
	movq $0, %rax
	call printf
	jmp 7f
1:
	cmpq $1, 8(%rdi)
	je print_true
	movq $.false, %rdi
	movq $0, %rax
	call printf
	jmp 7f
print_true:
	movq $.true, %rdi
	movq $0, %rax
	call printf
	jmp 7f
2:
	movq 8(%rdi), %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	jmp 7f
3:
	addq $16, %rdi
	movq %rdi, %rsi
	movq $.Sprint_str, %rdi
	movq $0, %rax
	call printf
	jmp 7f
7:
	popq %rax
	popq %rbp
	ret
num_modulo:
	pushq %rbp
	movq %rsp, %rbp
	movq 24(%rbp), %rsi
	movq 8(%rsi), %rax
	movq 32(%rbp), %rsi
	movq 8(%rsi), %rdx
	movq $0, %rsi
	cmpq $0, %rdx
	jg 1f
	movq $1, %rsi
	negq %rdx
	negq %rax
1:
	cmpq $0, %rax
	jg 1f
	addq %rdx, %rax
	jmp 1b
1:
	cmpq %rax, %rdx
	jg 1f
	subq %rdx, %rax
	jmp 1b
1:
	cmpq $0, %rsi
	je 1f
	negq %rax
1:
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdi
	movq $2, 0(%rax)
	movq %rdi, 8(%rax)
	popq %rbp
	ret
	.data
.Sprint_int:
	.string "%d"
.Sprint_str:
	.string "%s"
.true:
	.string "true"
.false:
	.string "false"
.nothing:
	.string "nothing"
