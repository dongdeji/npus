// See LICENSE for license details.
#include <stdint.h>


#ifndef ROCC_SOFTWARE_SRC_XCUSTOM_H_
#define ROCC_SOFTWARE_SRC_XCUSTOM_H_

#define STR1(x) #x
#ifndef STR
#define STR(x) STR1(x)
#endif
#define EXTRACT(a, size, offset) (((~(~0 << size) << offset) & a) >> offset)

// rd = rs2[offset + size - 1 : offset]
// rs1 is clobbered
// rs2 is left intact
#define EXTRACT_RAW(rd, rs1, rs2, size, offset) \
  not x ## rs1, x0;                             \
  slli x ## rs1, x ## rs1, size;                \
  not x ## rs1, x ## rs1;                       \
  slli x ## rs1, x ## rs1, offset;              \
  and x ## rd, x ## rs1, x ## rs2;              \
  srai x ## rd, x ## rd, offset;

#define XCUSTOM_OPCODE(x) XCUSTOM_OPCODE_ ## x
#define XCUSTOM_OPCODE_0 0b0001011
#define XCUSTOM_OPCODE_1 0b0101011
#define XCUSTOM_OPCODE_2 0b1011011
#define XCUSTOM_OPCODE_3 0b1111011

#define XCUSTOM(x, rd, rs1, rs2, funct)         \
  XCUSTOM_OPCODE(x)                   |         \
  (rd                   << (7))       |         \
  (0x3                  << (7+5))     |         \
  ((rd != 0) & 1        << (7+5+2))   |         \
  (rs1                  << (7+5+3))   |         \
  (rs2                  << (7+5+3+5)) |         \
  (EXTRACT(funct, 7, 0) << (7+5+3+5+5))

#define ROCC_INSTRUCTION_RAW_R_R_R(x, rd, rs1, rs2, funct)      \
  .word XCUSTOM(x, ## rd, ## rs1, ## rs2, funct)

// Standard macro that passes rd, rs1, and rs2 via registers
#define ROCC_INSTRUCTION(x, rd1, rd2, rd3, rd4, rs11, rs12, rs13, rs14, rs21, rs22, rs23, rs24, funct)                \
  ROCC_INSTRUCTION_R_R_R(x, rd1, rd2, rd3, rd4, \
                            rs11, rs12, rs13, rs14, \
                            rs21, rs22, rs23, rs24, \
                            funct, \
                            10, 11, 12, 13, \
                            14, 15, 16, 17, \
                            18, 19, 20, 21)

// rd, rs1, and rs2 are data
// rd_n, rs_1, and rs2_n are the register numbers to use
#define ROCC_INSTRUCTION_R_R_R(x, rd1, rd2, rd3, rd4, \
                                  rs11, rs12, rs13, rs14, \
                                  rs21, rs22, rs23, rs24, \
                                  funct, \
                                  rd1_n,  rd2_n,  rd3_n,  rd4_n,\
                                  rs11_n, rs12_n, rs13_n, rs14_n, \
                                  rs21_n, rs22_n, rs23_n, rs24_n) \
  {                                                                     \
    register uint64_t rd1_  asm ("x" # rd1_n);                          \
    register uint64_t rd2_  asm ("x" # rd2_n);                          \
    register uint64_t rd3_  asm ("x" # rd3_n);                          \
    register uint64_t rd4_  asm ("x" # rd4_n);                          \
    register uint64_t rs11_ asm ("x" # rs11_n) = (uint64_t) rs11;        \
    register uint64_t rs12_ asm ("x" # rs12_n) = (uint64_t) rs12;        \
    register uint64_t rs13_ asm ("x" # rs13_n) = (uint64_t) rs13;        \
    register uint64_t rs14_ asm ("x" # rs14_n) = (uint64_t) rs14;        \
    register uint64_t rs21_ asm ("x" # rs21_n) = (uint64_t) rs21;        \
    register uint64_t rs22_ asm ("x" # rs22_n) = (uint64_t) rs22;        \
    register uint64_t rs23_ asm ("x" # rs23_n) = (uint64_t) rs23;        \
    register uint64_t rs24_ asm ("x" # rs24_n) = (uint64_t) rs24;        \
    asm volatile (                                                      \
        ".word " STR(XCUSTOM(x, rd1_n, rs11_n, rs21_n, funct)) "\n\t"   \
        : "=r" (rd1_) ,"=r" (rd2_) ,"=r" (rd3_) ,"=r" (rd4_)            \
        : [_rs11] "r" (rs11_), [_rs12] "r" (rs12_), [_rs13] "r" (rs13_), [_rs14] "r" (rs14_),  \
          [_rs21] "r" (rs21_), [_rs22] "r" (rs22_), [_rs23] "r" (rs23_), [_rs24] "r" (rs24_)); \
    rd1 = rd1_;                                                         \
    rd2 = rd2_;                                                         \
    rd3 = rd3_;                                                         \
    rd4 = rd4_;                                                         \
  }

#define ROCC_INSTRUCTION_0_R_R(x, rs1, rs2, funct, rs1_n, rs2_n)  \
  {                                                               \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;    \
    register uint64_t rs2_ asm ("x" # rs2_n) = (uint64_t) rs2;    \
    asm volatile (                                                \
        ".word " STR(XCUSTOM(x, 0, rs1_n, rs2_n, funct)) "\n\t"   \
        :: [_rs1] "r" (rs1_), [_rs2] "r" (rs2_));                 \
  }

// [TODO] fix these to align with the above approach
// Macro to pass rs2_ as an immediate
/*
#define ROCC_INSTRUCTION_R_R_I(XCUSTOM_, rd_, rs1_, rs2_, funct_) \
  asm volatile (XCUSTOM_" %[rd], %[rs1], %[rs2], %[funct]"        \
                : [rd] "=r" (rd_)                                 \
                : [rs1] "r" (rs1_), [rs2] "i" (rs2_), [funct] "i" (funct_))
// Macro to pass rs1_ and rs2_ as immediates
#define ROCC_INSTRUCTION_R_I_I(XCUSTOM_, rd_, rs1_, rs2_, funct_) \
  asm volatile (XCUSTOM_" %[rd], %[rs1], %[rs2], %[funct]"        \
                : [rd] "=r" (rd_)                                 \
                : [rs1] "i" (rs1_), [rs2] "i" (rs2_), [funct] "i" (funct_))
*/

#endif  // ROCC_SOFTWARE_SRC_XCUSTOM_H_

#define doCustom(y1, y2, y3, y4, data1, data2, data3, data4, data5, data6, data7, data8) \
        ROCC_INSTRUCTION(3, y1, y2, y3, y4, data1, data2, data3, data4, data5, data6, data7, data8, 0)

/* Register offsets */
#define UART_REG_TXFIFO         0x00
#define UART_REG_RXFIFO         0x04
#define UART_REG_TXCTRL         0x08
#define UART_REG_RXCTRL         0x0c
#define UART_REG_IE             0x10
#define UART_REG_IP             0x14
#define UART_REG_DIV            0x18

#define REG32(p, i)	((p)[(i) >> 2])
static volatile uint32_t * const uart = (void *)(0x54000000);

volatile void kputc(char c)
{
	volatile uint32_t *tx = &REG32(uart, UART_REG_TXFIFO);

	while ((int32_t)(*tx) < 0);
	*tx = c;
}

volatile void _kputs(const char *s)
{
	char c;
	for (; (c = *s) != '\0'; s++)
		kputc(c);
}


int main(void)
{
  //uint64_t data [] = { 0x1111, 0x2222, 0x3333, 0x4444, \
  //                     0x5555, 0x6666, 0x7777, 0x8888, \
  //                     0x9999, 0xaaaa, 0xbbbb, 0xcccc, \
  //                     0xdddd, 0xeeee, 0xffff, 0x5a5a };
  //uint64_t y1, y2, y3, y4, y5, y6, y7, y8 ;
  while(1)
  {
    //doCustom(y1, y2, y3, y4, data[0], data[1],  data[2],  data[3],  data[4],  data[5],  data[6],  data[7]);
    //doCustom(y5, y6, y7, y8, data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15]);
    _kputs("Hellow World\r\n");
  }
	return 0;
}
