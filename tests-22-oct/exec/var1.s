	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $41, 8(%rax)
	pushq %rax
	movq -8(%rbp), %rax
	movq %rax, %rdi
	call print
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $42, 8(%rax)
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
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
	cmpq $1, 0(%rdi)
	je 1f
	cmpq $2, 0(%rdi)
	je 2f
	cmpq $3, 0(%rdi)
	je 3f
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
	.data
.Sprint_int:
	.string "%d"
.Sprint_str:
	.string "%s"
.true:
	.string "true"
.false:
	.string "false"
