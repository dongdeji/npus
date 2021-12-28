// See LICENSE for license details.
#ifndef _NPINSTR_H
#define _NPINSTR_H

#define EXTRACT(a, size, offset) (((~(~0 << size) << offset) & a) >> offset)

#define XCUSTOM_OPCODE(x) XCUSTOM_OPCODE_ ## x
#define XCUSTOM_OPCODE_0 0b0001011
#define XCUSTOM_OPCODE_1 0b0101011

// | b31~b28     | b27~b25   | b24~b20  | b19~b15    | b14~12     | b11~b7 | b6~b0             |
// | funct4:0000 | func3:000 | imm[9:5] | imm[14:10] | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to pkt wind + jal
// | funct4:0000 | func3:001 | imm[9:5] | imm[14:10] | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to result wind + jal
// | funct4:0000 | func3:010 | imm[9:5] | imm[14:10] | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to rd + jal
// | funct4:0001 | func3:000 | imm[9:5] | rs1        | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to pkt wind + jalr
// | funct4:0001 | func3:001 | imm[9:5] | rs1        | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to result wind + jalr
// | funct4:0001 | func3:010 | imm[9:5] | rs1        | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to rd + jalr
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:000 | rd     | coustom0: 0001011 | -- swap pkt wind byte
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:001 | rd     | coustom0: 0001011 | -- swap pkt wind halfword
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:010 | rd     | coustom0: 0001011 | -- swap pkt wind word
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:011 | rd     | coustom0: 0001011 | -- swap pkt wind doubleword
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:100 | rd     | coustom0: 0001011 | -- swap pkt wind unsigned byte
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:101 | rd     | coustom0: 0001011 | -- swap pkt wind unsigned halfword
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:110 | rd     | coustom0: 0001011 | -- swap pkt wind unsigned word
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:000 | rd     | coustom0: 0001011 | -- swap result wind byte
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:001 | rd     | coustom0: 0001011 | -- swap result wind halfword
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:010 | rd     | coustom0: 0001011 | -- swap result wind word
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:011 | rd     | coustom0: 0001011 | -- swap result wind doubleword
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:100 | rd     | coustom0: 0001011 | -- swap result wind unsigned byte
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:101 | rd     | coustom0: 0001011 | -- swap result wind unsigned halfword
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:110 | rd     | coustom0: 0001011 | -- swap result wind unsigned word

#define LXJAL(func3, imm, rd)                       \
       (XCUSTOM_OPCODE(0)       << (0))          |  \
       (rd                      << (7))          |  \
       (EXTRACT(imm, 4, 2)      << (7+5))        |  \
       (EXTRACT(imm,14,10)      << (7+5+3))      |  \
       (EXTRACT(imm, 9, 5)      << (7+5+3+5))    |  \
       (func3                   << (7+5+3+5+5))  |  \
       (0                       << (7+5+3+5+5+3))

#define LXJALR(func3, imm, rd, rs1)                 \
       (XCUSTOM_OPCODE(0)       << (0))          |  \
       (rd                      << (7))          |  \
       (EXTRACT(imm, 4, 2)      << (7+5))        |  \
       (rs1                     << (7+5+3))      |  \
       (EXTRACT(imm, 9, 5)      << (7+5+3+5))    |  \
       (func3                   << (7+5+3+5+5))  |  \
       (1                       << (7+5+3+5+5+3))

#define SWAPXX(func3, woffset, Usize, rd)           \
       (XCUSTOM_OPCODE(0)       << (0))          |  \
       (rd                      << (7))          |  \
       (Usize                   << (7+5))        |  \
       (EXTRACT(woffset, 9, 0)  << (7+5+3))      |  \
       (func3                   << (7+5+3+10))   |  \
       (2                       << (7+5+3+10+3))


#define LPKTWJAL(imm, rd)            .word LXJAL(0, imm, ## rd)
#define LRESWJAL(imm, rd)            .word LXJAL(1, imm, ## rd)
#define LRDJAL(imm, rd)              .word LXJAL(2, imm, ## rd)
#define LPKTWJALR(imm, rd, rs1)      .word LXJALR(0, imm, ## rd, ## rs1)
#define LRESWJALR(imm, rd, rs1)      .word LXJALR(1, imm, ## rd, ## rs1)
#define LRDJALR(imm, rd, rs1)        .word LXJALR(2, imm, ## rd, ## rs1)
#define SWAPPKTB(woffset, rd)        .word SWAPXX(0, woffset, 0, ## rd)
#define SWAPPKTH(woffset, rd)        .word SWAPXX(0, woffset, 1, ## rd)
#define SWAPPKTW(woffset, rd)        .word SWAPXX(0, woffset, 2, ## rd)
#define SWAPPKTD(woffset, rd)        .word SWAPXX(0, woffset, 3, ## rd)
#define SWAPPKTUB(woffset, rd)       .word SWAPXX(0, woffset, 4, ## rd)
#define SWAPPKTUH(woffset, rd)       .word SWAPXX(0, woffset, 5, ## rd)
#define SWAPPKTUW(woffset, rd)       .word SWAPXX(0, woffset, 6, ## rd)
#define SWAPRESB(woffset, rd)        .word SWAPXX(1, woffset, 0, ## rd)
#define SWAPRESH(woffset, rd)        .word SWAPXX(1, woffset, 1, ## rd)
#define SWAPRESW(woffset, rd)        .word SWAPXX(1, woffset, 2, ## rd)
#define SWAPRESD(woffset, rd)        .word SWAPXX(1, woffset, 3, ## rd)
#define SWAPRESUB(woffset, rd)       .word SWAPXX(1, woffset, 4, ## rd)
#define SWAPRESUH(woffset, rd)       .word SWAPXX(1, woffset, 5, ## rd)
#define SWAPRESUW(woffset, rd)       .word SWAPXX(1, woffset, 6, ## rd)

#define XCUSTOM(x, rd, rs1, rs2, funct)         \
  (XCUSTOM_OPCODE(x)    << (0))       |         \
  (rd                   << (7))       |         \
  (0x3                  << (7+5))     |         \
  ((rd != 0) & 1        << (7+5+2))   |         \
  (rs1                  << (7+5+3))   |         \
  (rs2                  << (7+5+3+5)) |         \
  (EXTRACT(funct, 7, 0) << (7+5+3+5+5))

#define ROCC_INSTRUCTION_RAW_R_R_R(x, rd, rs1, rs2, funct)      \
  .word XCUSTOM(x, ## rd, ## rs1, ## rs2, funct)


#endif
