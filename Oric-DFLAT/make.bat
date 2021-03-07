echo off
REM ** Make sure this path is right for your location **
cd C:\Users\Dolo\Documents\Home Brew Computer\Oric-DFLAT
setlocal EnableDelayedExpansion


:compile
color 81
echo Assembly started   : %date% %time%

REM Final binary which include the real extern addresses and correct bank numbers
echo Generating final symbol tables for externals

REM Build with -l option to get full listing
echo Building ROM

rem as65 -n -c -l -t -dBANK0 -orom\bank0.bin bank\bank0.s
as65 -n -c -l -t -dBANK0 -orom\bank0.bin bank\bank0.s
if errorlevel 1 goto errors
rem copy bank\bank0.lst bank\bank0.sym

REM Combine individual banks in to one 64K binary for EEPROM programming
copy /Y/B rom\bank0.bin+rom\bank0.bin+rom\bank0.bin+rom\bank0.bin rom\ORICD.ROM

REM Copy bank 0 to Oricutron folder as dflat.rom
copy /Y/B rom\bank0.bin emulator\roms\dflat.rom

REM Copy tt_ and fd_ symbols to the Oricutron rom file
echo tt_readbyte_setcarry = no > emulator\roms\dflat.pch
for /f "tokens=1,3" %%e in ('findstr "tt_" bank\bank0.lst ') do (
	echo %%e = %%f >> emulator\roms\dflat.pch
	)
for /f "tokens=1,3" %%e in ('findstr "fd_" bank\bank0.lst ') do (
	echo %%e = %%f >> emulator\roms\dflat.pch
	)

REM The symbol file does not seem to work yet
REM Copy all symbols to the Oricutron symbol file
REM del emulator\roms\dflat.sym
REM for /f "tokens=1,3" %%e in ('findstr ": $" bank\bank0.lst ') do (
REM 	echo %%f %%e >> emulator\roms\dflat.sym
REM 	)

color 21
echo COMPILE SUCCESSFUL : !date! !time!
goto end

:errors
color 41
echo STOPPED DUE TO ASSEMBLY ERRORS
goto end

:end


