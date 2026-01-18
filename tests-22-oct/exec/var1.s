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
	movq $6, 0(%rax)
	leaq each(%rip), %rdx
	movq %rdx, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $6, 0(%rax)
	leaq fold(%rip), %rdx
	movq %rdx, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $41, 8(%rax)
	pushq %rax
	movq -72(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	addq $16, %rax
	pushq %rax
	call *-8(%rax)
	popq %rdi
	popq %rdi
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $42, 8(%rax)
	movq %rax, -72(%rbp)
	movq -72(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	addq $16, %rax
	pushq %rax
	call *-8(%rax)
	popq %rdi
	popq %rdi
	movq $18, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movb $10, 16(%rax)
	movq $1, 8(%rax)
	movb $0, 17(%rax)
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
	cmpq $4, 0(%rdi)
	je 4f
	cmpq $5, 0(%rdi)
	je 4f
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
4:
	pushq %rbx
	movq %rdi, %rbx
	movq $.Sprint_list, %rdi
	movq $0, %rax
	call printf
	cmpq $4, 0(%rbx)
	je fin_print_list
	movq 8(%rbx), %rdi
	pushq %rdi
	pushq %rdi
	call print
	addq $16, %rsp
	movq 16(%rbx), %rbx
boucle_print_list:
	cmpq $4, 0(%rbx)
	je fin_print_list
	movq $.Sprint_sep, %rdi
	movq $0, %rax
	call printf
	movq 8(%rbx), %rdi
	pushq %rdi
	pushq %rdi
	call print
	addq $16, %rsp
	movq 16(%rbx), %rbx
	jmp boucle_print_list
fin_print_list:
	movq $.Sprint_fin_list, %rdi
	movq $0, %rax
	call printf
	popq %rbx
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
	movq 8(%rsi), %r12
	movq $0, %rsi
	cmpq $0, %r12
	jg 1f
	movq $1, %rsi
	negq %r12
	negq %rax
1:
	cqto
	idivq %r12
	cmpq $0, %rdx
	jge 1f
	addq %r12, %rdx
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
	addq $16, %rax
	addq $16, %rdx
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
each:
	pushq %rbp
	movq %rsp, %rbp
	pushq %rbx
	pushq %r12
	movq 24(%rbp), %rbx
	addq $16, %rbx
	movq 32(%rbp), %r12
1:
	movq 0(%r12), %rdi
	cmpq $4, %rdi
	je 2f
	movq 8(%r12), %rdx
	pushq %rdx
	pushq %rbx
	call *-8(%rbx)
	addq $16, %rsp
	movq 16(%r12), %r12
	jmp 1b
2:
	movq $8, %rdi
	call my_malloc
	movq $0, 0(%rax)
	popq %r12
	popq %rbx
	popq %rbp
	ret
fold:
	pushq %rbp
	movq %rsp, %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	movq 24(%rbp), %rbx
	addq $16, %rbx
	movq 32(%rbp), %r13
	movq 40(%rbp), %r12
1:
	movq 0(%r12), %rdi
	cmpq $4, %rdi
	je 2f
	movq 8(%r12), %rdx
	pushq %rdx
	pushq %r13
	pushq %rbx
	call *-8(%rbx)
	movq %rax, %r13
	addq $24, %rsp
	movq 16(%r12), %r12
	jmp 1b
2:
	movq %r13, %rax
	popq %r13
	popq %r12
	popq %rbx
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
.Sprint_list:
	.string "[list: "
.Sprint_fin_list:
	.string "]"
.Sprint_sep:
	.string ", "
