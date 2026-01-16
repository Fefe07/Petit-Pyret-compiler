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
	movq 32(%rbp), %rax
	pushq 8(%rax)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $0, 8(%rax)
	movq 8(%rax), %rdx
	popq %rax
	cmpq %rdx, %rax
	jne l23
	movq $1, %rax
	jmp l24
l23:
	movq $0, %rax
l24:
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, %rdi
	movq %rdi, 0(%rax)
	popq %rdi
	movq %rdi, 8(%rax)
	cmpq $0, 8(%rax)
	jne 1f
	movq 32(%rbp), %rax
	pushq %rax
	movq 24(%rbp), %rax
	pushq %rax
	call *8(%rax)
	popq %rdi
	popq %rdi
	movq 32(%rbp), %rax
	pushq 8(%rax)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq 8(%rax), %rdx
	popq %rax
	subq %rdx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	popq %rdi
	movq %rdi, 8(%rax)
	pushq %rax
	movq 24(%rbp), %rax
	pushq %rax
	movq 16(%rbp), %rdi
	movq 8(%rdi), %rax
	pushq %rax
	call *8(%rax)
	popq %rdi
	popq %rdi
	popq %rdi
	jmp 2f
1:
	movq 16(%rbp), %rdi
	movq 0(%rdi), %rax
2:
	movq %rbp, %rsp
	popq %rbp
	ret
l21:
	movq $32, %rdi
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
	movq -24(%rbp), %rax
	popq %rdi
	popq %rax
	movq %rdi, 24(%rax)
	pushq %rax
	jmp l25
l26:
	pushq %rbp
	movq %rsp, %rbp
	movq 24(%rbp), %rax
	movq %rax, %rdi
	call print
	movq $18, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movb $10, 16(%rax)
	movq $1, 8(%rax)
	movb $0, 17(%rax)
	movq %rax, %rdi
	call print
	movq 16(%rbp), %rdi
	movq 0(%rdi), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
l25:
	movq $24, %rdi
	call my_malloc
	movq $6, 0(%rax)
	leaq l26(%rip), %rdx
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
	movq $10, 8(%rax)
	pushq %rax
	movq -32(%rbp), %rax
	pushq %rax
	movq -24(%rbp), %rax
	pushq %rax
	call *8(%rax)
	popq %rdi
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
