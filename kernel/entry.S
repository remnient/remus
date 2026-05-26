#include <linux/linkage.h>
#include <asm/unwind_hints.h>
#include <asm/ibt.h>

.text

SYM_FUNC_START(remus_launch)
  push %rbp
  push %rbx
  push %r12
  push %r13
  push %r14
  push %r15
  UNWIND_HINT_SAVE
  mov $0x681c, %rax
  vmwrite %rsp, %rax
  jna .Llaunch_failed
  lea .Lguest_started(%rip), %rcx
  mov $0x681e, %rax
  vmwrite %rcx, %rax
  jna .Llaunch_failed
  pushfq
  pop %rcx
  mov $0x6820, %rax
  vmwrite %rcx, %rax
  jna .Llaunch_failed
  vmlaunch
.Llaunch_failed:
  mov $-5, %eax
  jmp .Llaunch_return
.Lguest_started:
  UNWIND_HINT_RESTORE
  xor %eax, %eax
.Llaunch_return:
  pop %r15
  pop %r14
  pop %r13
  pop %r12
  pop %rbx
  pop %rbp
  RET
SYM_FUNC_END(remus_launch)

.macro restore_registers
  mov 0(%rdi), %rax
  mov 8(%rdi), %rcx
  mov 16(%rdi), %rdx
  mov 24(%rdi), %rbx
  mov 40(%rdi), %rbp
  mov 48(%rdi), %rsi
  mov 64(%rdi), %r8
  mov 72(%rdi), %r9
  mov 80(%rdi), %r10
  mov 88(%rdi), %r11
  mov 96(%rdi), %r12
  mov 104(%rdi), %r13
  mov 112(%rdi), %r14
  mov 120(%rdi), %r15
  mov 56(%rdi), %rdi
.endm

.macro save_registers
  push %rax
  mov 8(%rsp), %rax
  mov %rcx, 8(%rax)
  mov %rdx, 16(%rax)
  mov %rbx, 24(%rax)
  mov %rbp, 40(%rax)
  mov %rsi, 48(%rax)
  mov %rdi, 56(%rax)
  mov %r8, 64(%rax)
  mov %r9, 72(%rax)
  mov %r10, 80(%rax)
  mov %r11, 88(%rax)
  mov %r12, 96(%rax)
  mov %r13, 104(%rax)
  mov %r14, 112(%rax)
  mov %r15, 120(%rax)
  mov (%rsp), %rcx
  mov %rcx, (%rax)
  mov %rax, %rdi
  add $8, %rsp
.endm

SYM_FUNC_START(remus_forward_vmcall)
  push %rbp
  push %rbx
  push %r12
  push %r13
  push %r14
  push %r15
  push %rdi
  restore_registers
  vmcall
  save_registers
  add $8, %rsp
  pop %r15
  pop %r14
  pop %r13
  pop %r12
  pop %rbx
  pop %rbp
  RET
SYM_FUNC_END(remus_forward_vmcall)

SYM_CODE_START(remus_entry)
  UNWIND_HINT_END_OF_STACK
  ENDBR
  save_registers
  call remus_dispatch
  mov (%rsp), %rdi
  test %eax, %eax
  jz .Lrestore_host
  restore_registers
  vmresume
  mov $0x4400, %rax
  vmread %rax, %rsi
  lea .Lresume_error(%rip), %rdi
  xor %eax, %eax
  push $0
  UNWIND_HINT_END_OF_STACK
  jmp panic
.Lrestore_host:
  call remus_restore
  mov (%rsp), %rdi
  sub $40, %rsp
  mov 128(%rdi), %rax
  mov %rax, 0(%rsp)
  mov 152(%rdi), %rax
  mov %rax, 8(%rsp)
  mov 144(%rdi), %rax
  mov %rax, 16(%rsp)
  mov 136(%rdi), %rax
  mov %rax, 24(%rsp)
  mov 160(%rdi), %rax
  mov %rax, 32(%rsp)
  restore_registers
  UNWIND_HINT_IRET_REGS
  iretq
SYM_CODE_END(remus_entry)

.section .rodata
.Lresume_error:
  .asciz "remus: vmresume failed with error %lu"

.section .note.GNU-stack,"",@progbits
