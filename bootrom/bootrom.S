#define DRAM_BASE 0x80000000

#include "npinstr.h"

.section .text.start, "ax", @progbits
.globl _start
_start:
  //csrwi 0x7c1, 0 // disable chicken bits
  //csrr a0, mhartid
  LPKTWJAL(4, 0)
  SWAPPKTH(0, 1)
  STOREKEYHU(1)
  STOREKEYHU(1)
  STOREKEYHU(1)
  STOREKEYHU(1)

  LKRDJAL(4,2,0)

  SWAPPKTH(0, 1)
  SWAPPKTW(0, 1)
  SWAPPKTD(0, 1)

  SWAPPKTB(1, 1)
  SWAPPKTH(1, 1)
  SWAPPKTW(1, 1)
  SWAPPKTD(1, 1)

  SWAPPKTB(2, 1)
  SWAPPKTH(2, 1)
  SWAPPKTW(2, 1)
  SWAPPKTD(2, 1)

  LPKTWJAL(4, 0)
  add  x0, x0, x0
  li   a0, 0x1  
  slli a1, a0, 12
  li   sp, 0x10000
  lb   a1, 9(sp)
  add  a2, a1, x0
  li   sp, 0x20ff0
  lb   a1, 0(sp)
  //csrr a0, mhartid
  call main
  //csrr a0, mhartid
  la a1, _dtb
  jr s0
  //LPKTWJAL(4, 0)


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
