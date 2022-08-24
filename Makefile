# Usage:
#   E.g. `make stripes` to run the program in content/jack/stripes

CARGO_RUN := cargo run --release --example

.PHONY: clean
.SECONDARY:

STD_SRC := $(wildcard content/jack/std/*.jack)
STD_VM_FILES := $(patsubst %.jack,%.vm,$(STD_SRC))
STD_VMS := $(patsubst content/jack/std/%.jack,%.vm,$(STD_SRC))

std: $(STD_VM_FILES)

clean:
	find content/jack -name "*.vm" -exec rm -rf {} \;
	find content/jack -name "*.asm" -exec rm -rf {} \;
	find content/jack -name "*.bin" -exec rm -rf {} \;

%: content/jack/%/main.bin
	cat $< | $(CARGO_RUN) hack_emulator

content/%/main.asm: content/%/Main.vm std
	cp $(STD_VM_FILES) content/$*
	$(CARGO_RUN) vm_to_asm content/$* > $@

%.vm: %.jack
	$(CARGO_RUN) jack_to_vm $<

%.bin: %.asm
	cat $< | $(CARGO_RUN) hasm > $@
