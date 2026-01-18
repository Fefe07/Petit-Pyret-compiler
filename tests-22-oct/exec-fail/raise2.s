	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call my_malloc
	movq $6, 0(%rax)
	leaq print(%rip), %rdx
	movq %rdx, 8(%rax)
	pushq %rax
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
	movq $8, %rdi
	call my_malloc
	movq $4, 0(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $6, 0(%rax)
	leaq link(%rip), %rdx
	movq %rdx, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $6, 0(%rax)
	leaq raise(%rip), %rdx
	movq %rdx, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq 8(%rax)
	movq $23, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movb $101, 16(%rax)
	movb $114, 17(%rax)
	movb $114, 18(%rax)
	movb $101, 19(%rax)
	movb $117, 20(%rax)
	movb $114, 21(%rax)
	movq $6, 8(%rax)
	movb $0, 22(%rax)
	pushq %rax
	movq -48(%rbp), %rax
	addq $16, %rax
	pushq %rax
	call *-8(%rax)
	popq %rdi
	popq %rdi
	movq 8(%rax), %rdx
	popq %rax
	addq %rdx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	popq %rdi
	movq %rdi, 8(%rax)
	pushq %rax
	movq -8(%rbp), %rax
	addq $16, %rax
	pushq %rax
	call *-8(%rax)
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
	movq %rsp, %rbp
	movq 24(%rbp), %rdi
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
	movq 24(%rbp), %rax
	popq %rbp
	ret
num_modulo:
	pushq %rbp
	movq %rsp, %rbp
	movq 24(%rbp), %rsi
	movq 8(%rsi), %rax
	movq 32(%rbp), %rsi
	movq 8(%rsi), %rbx
	movq $0, %rsi
	cmpq $0, %rbx
	jg 1f
	movq $1, %rsi
	negq %rbx
	negq %rax
1:
	cqto
	idivq %rbx
	cmpq $0, %rdx
	jge 1f
	addq %rbx, %rdx
1:
	movq %rdx, %rax
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
link:
	pushq %rbp
	movq %rsp, %rbp
	movq 24(%rbp), %r10
	movq 32(%rbp), %r11
	pushq %r10
	pushq %r11
	movq $24, %rdi
	call my_malloc
	popq %r11
	popq %r10
	movq $5, 0(%rax)
	movq %r10, 8(%rax)
	movq %r11, 16(%rax)
	movq %rbp, %rsp
	popq %rbp
	ret
equality:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rax
	movq 24(%rbp), %rdx
	movq 0(%rax), %rsi
	movq 0(%rdx), %rdi
	cmpq %rsi, %rdi
	jne 8f
	cmpq $0, %rsi
	je 9f
	cmpq $4, %rsi
	je 9f
	cmpq $1, %rsi
	je 1f
	cmpq $2, %rsi
	je 1f
	cmpq $3, %rsi
	je 3f
	cmpq $5, %rsi
	je 5f
	movq $60, %rax
	movq $2, %rdi
	syscall
1:
	movq 8(%rax), %rsi
	movq 8(%rdx), %rdi
	cmpq %rsi, %rdi
	je 9f
8:
	pushq $0
	jmp 7f
3:
	addq $8, %rax
	addq $8, %rdx
6:
	movb 0(%rax), %sil
	movb 0(%rdx), %dil
	addq $1, %rax
	addq $1, %rdx
	cmpb %sil, %dil
	jne 8b
	cmpb $0, %sil
	je 9f
	jmp 6b
5:
	pushq %rax
	pushq %rdx
	pushq 8(%rax)
	pushq 8(%rdx)
	call equality
	cmpq $0, 8(%rax)
	je 8b
	popq %rax
	popq %rax
	popq %rdx
	popq %rax
	pushq 16(%rax)
	pushq 16(%rdx)
	call equality
	cmpq $0, 8(%rax)
	je 8b
	jmp 9f
9:
	pushq $1
7:
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	popq %rdi
	movq %rdi, 8(%rax)
	movq %rbp, %rsp
	popq %rbp
	ret
raise:
	pushq %rbp
	movq %rsp, %rbp
	movq $60, %rax
	movq $2, %rdi
	syscall
	movq %rbp, %rsp
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
