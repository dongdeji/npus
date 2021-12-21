#define DRAM_BASE 0x80000000

.section .text.start, "ax", @progbits
.globl _start
_start:
  //csrwi 0x7c1, 0 // disable chicken bits
  //csrr a0, mhartid
  li   a0, 0x1  
  slli a1, a0, 12
  li   sp, 0x2000000
  add  x0, x0, x0
  add  x0, x0, x0
  add  sp, sp, a1
  lb   a1, 0(sp)
  csrr a0, mhartid
  call main
  csrr a0, mhartid
  la a1, _dtb
  jr s0

.section .text.hang, "ax", @progbits
.globl _hang
_hang:
  csrwi 0x7c1, 0 // disable chicken bits
  csrr a0, mhartid
  la a1, _dtb
  csrwi mie, 0
1:
  wfi
  j 1b

.section .rodata.dtb, "a", @progbits
.globl _dtb
.align 5, 0
_dtb:
.ascii "DTB goes here"
