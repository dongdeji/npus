bootrom_img = bootrom.img

GCC=riscv64-unknown-elf-gcc
OBJCOPY=riscv64-unknown-elf-objcopy
OBJDUMP=riscv64-unknown-elf-objdump

all: $(bootrom_img)

%.img: %.bin
	dd if=$< of=$@ bs=1024 count=8

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@
	$(OBJDUMP) -D -S  $< > $<.dump
	$(OBJDUMP) -D -M no-aliases,numeric $< > $<.no-aliases,numeric.dump

%.elf: %.S main.c linker.ld
	$(GCC) -march=rv64i -mcmodel=medany -O2 -std=gnu11 -Wall -nostartfiles -fno-common -g -DENTROPY=0 -mabi=lp64 -DNONSMP_HART=0 -Tlinker.ld $< main.c -nostdlib -static -Wl,--no-gc-sections -o $@


clean:
	rm -rf *.img *.dump


