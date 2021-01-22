# commmnd line tools
# dftap2txt := ./emulator/Util/build/dftap2txt/dftap2txt/dftap2txt
# dftap2wav := ./emulator/Util/build/dftap2wav/dftap2wav/dftap2wav
# dftxt2tap := ./emulator/Util/build/dftxt2tap/dftxt2tap/dftxt2tap

all: tools rom

.PHONY: clean rom tools

clean:
	@rm -rf emulator/Util/build/*


rom:
	@./as65 -n -c -l -t -dBANK0 -orom/bank0.bin bank/bank0.s
	# Combine individual banks in to one 64K binary for EEPROM programming
	@cat rom/bank0.bin rom/bank0.bin rom/bank0.bin rom/bank0.bin > rom/ORICD.ROM
	# Copy bank 0 to Oricutron folder as dflat.rom
	@cat rom/bank0.bin > emulator/roms/dflat.rom
	# Copy tt_ and fd_ symbols to the Oricutron rom file
	@echo tt_readbyte_setcarry = no > emulator/roms/dflat.pch
	@cat bank/bank0.lst|grep '^\ *tt_'|sed -e 's/^\ *//g' -e 's/:/=/g'|cut -f1,2,3 -d' ' >> emulator/roms/dflat.pch
	@cat bank/bank0.lst|grep '^\ *fd_'|sed -e 's/^\ *//g' -e 's/:/=/g'|cut -f1,2,3 -d' ' >> emulator/roms/dflat.pch
	# Copy all symbols to the Oricutron symbol file
# 	@ cat bank/bank0.lst > emulator/roms/dflat.sym

tools: clean
	@mkdir -p emulator/Util/build
	@cd emulator/Util/build/ && cmake .. && make

