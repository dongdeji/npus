#
# Building Chisel examples without too much sbt/scala/... stuff
#
# sbt looks for default into a folder ./project and . for build.sdt and Build.scala
# sbt creates per default a ./target folder

SBT = sbt


init:
	git submodule update --init && git -C rocket-chip submodule update --init


# Generate Verilog code
thread:
	$(SBT) "runMain npus.ThreadMain"

npusTop:
	mkdir -p generated-src
	$(SBT) "runMain npus.TopMain -td generated-src --full-stacktrace --output-file npusTop.v --infer-rw --repl-seq-mem -c:npus.TopMain:-o:generated-src/npusTop.v.conf "
	./scripts/vlsi_mem_gen generated-src/npusTop.v.conf --tsmc28 --output_file generated-src/tsmc28_sram.v > generated-src/tsmc28_sram.v.conf
	./scripts/vlsi_mem_gen generated-src/npusTop.v.conf --output_file generated-src/sim_sram.v

# Generate run vcd
thread-test:
	$(SBT) "test:runMain npus.ThreadTester"


include $(abspath .)/Makefrag-verilator

# clean everything (including IntelliJ project settings)

clean:
	git clean -fd

