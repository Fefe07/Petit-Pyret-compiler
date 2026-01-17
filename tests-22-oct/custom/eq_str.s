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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movb $98, 16(%rax)
	movb $111, 17(%rax)
	movb $110, 18(%rax)
	movb $106, 19(%rax)
	movb $111, 20(%rax)
	movb $117, 21(%rax)
	movb $114, 22(%rax)
	movq $7, 8(%rax)
	movb $0, 23(%rax)
	pushq %rax
	movq $19, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movb $52, 16(%rax)
	movb $50, 17(%rax)
	movq $2, 8(%rax)
	movb $0, 18(%rax)
	popq %rdx
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
9:
	pushq $1
7:
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	popq %rdi
	movq %rdi, 8(%rax)
	movq %rax, %rdi
	call print
	movq $21, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movb $116, 16(%rax)
	movb $101, 17(%rax)
	movb $115, 18(%rax)
	movb $116, 19(%rax)
	movq $4, 8(%rax)
	movb $0, 20(%rax)
	pushq %rax
	movq $21, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movb $116, 16(%rax)
	movb $101, 17(%rax)
	movb $115, 18(%rax)
	movb $116, 19(%rax)
	movq $4, 8(%rax)
	movb $0, 20(%rax)
	popq %rdx
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
9:
	pushq $1
7:
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	popq %rdi
	movq %rdi, 8(%rax)
	movq %rax, %rdi
	call print
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
