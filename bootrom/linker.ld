SECTIONS
{
    ROM_BASE = 0x00000; /* ... but actually position independent */

    . = ROM_BASE;
    .text.start : { *(.text.start) }
    . = ROM_BASE + 0x400;
    .text.hang : { *(.text.hang) }
    . = ROM_BASE + 0x800;
    .rodata.dtb : { *(.rodata.dtb) }
}
