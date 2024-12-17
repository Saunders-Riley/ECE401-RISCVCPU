LIBFPGA_SOURCES := libfpga/mem/sram_sync.v \
					libfpga/mem/sram_sync_1r1w.v \
					libfpga/mem/ahb_sync_sram.v \
					libfpga/mem/cache_mem_set_associative.v \
					libfpga/mem/ahb_cache_writeback.v \
					libfpga/mem/ahb_cache_readonly.v 

OUTPUTS := riscv_fibonacci.out \
			riscv_fibonacci.mem \
			riscv_cpu_core.o

all: $(OUTPUTS)

clean:
	rm $(OUTPUTS)
	
riscv_cpu_core.o : riscv_cpu_core.sv
	iverilog -o $@ $< $(LIBFPGA_SOURCES)

riscv_fibonacci.out : riscv_fibonacci.asm
	riscv64-unknown-elf-as -o $@ $<

riscv_fibonacci.mem : riscv_fibonacci.out
	riscv64-unknown-elf-objcopy -O verilog --verilog-data-width=4 $< $@
