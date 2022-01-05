#define DRAM_BASE 0x80000000

#include "npinstr.h"

.section .text.start, "ax", @progbits
.globl _start
_start:
  //jal test1
  li x1,0x00450008        // ipv4 key
  li x2,0x11              // udp proto type key 
  li x3,0x12b5            // udp destport 4789(=0x12b5) for vxlan
  LPKTWJAL(4, 0)          // load eth pkt to pkt window and jump to +4, that is next instruction
startcheck:   
  SWAPPKTW(12, 4)         // offset 12byte swap 4bytes to x4
  bne x4, x1, notipv4     // compare to ipv4 key
  SWAPPKTB(23, 4)         // offset 23byte(ip proto) swap 1byte to x4
  bne x4, x2, notudp      // compare to udp proto type key
  SWAPPKTH(36, 4)         // offset 36byte(udp port) swap 2byte to x4
  bne x4, x3, notvxlan    // compare to ipv4 key
make5keys:   
  SWAPPKTD(26, 5)         // swap src+dest ip to x5
  STOREKEYDU(5)           // store src+dest ip in x5 to key buffer
  SWAPPKTW(34, 5)         // swap src+dest port to x5
  STOREKEYWU(5)           // store src+dest ip in x5 to key buffer
  LKRDJAL(4, 5, 0/*cam*/) // load key to cam, and retun result to x5
  // .... do other thing
reloadpkt:
  LPKTWJAL((startcheck-reloadpkt), 0) // reload new pkt to window, and jump to startcheck

notipv4:
notudp:
notvxlan:
//loop:
//  jal loop

test1:
  add  x0, x0, x0
  li   a0, 0x1  
  slli a1, a0, 12
  li   sp, 0x10000
  lbu  a1, 9(sp)
  add  a2, a1, x0
  li   sp, 0x20ff0
  lb   a1, 0(sp)
  //csrr a0, mhartid
  call main
  //csrr a0, mhartid
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
