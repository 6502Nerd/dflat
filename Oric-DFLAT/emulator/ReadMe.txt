Oricutron 1.2
-------------

(c)2009-2014 Peter Gordon (pete@petergordon.org.uk)

This is a work in progress.


Current status
==============

  6502:  100% done (apart from any unknown bugs :)
  VIA:   95% done.
  AY:    99% done.
  Video: 100% done
  Tape:  99% done (.TAP, .ORT and .WAV supported)
  Disk:  90% done (single-density mode not supported)



Credits
=======

  Programming
  -----------

  Peter Gordon


  Additional Programming
  ----------------------

  Francois Revol
  Alexandre Devert
  Stefan Haubenthal
  Ibisum
  Kamel Biskri
  Iss
  Christian from defence-force forum


  Amiga & Windows ports
  ---------------------

  Peter Gordon


  BeOS/Haiku port
  ---------------

  Francois Revol


  macOS port
  ----------

  Francois Revol
  Kamel Biskri
  Patrice Torguet


  MorphOS & AROS ports
  --------------------

  Stefan Haubenthal


  Linux port
  ----------

  Francois Revol
  Ibisum
  Alexandre Devert


  Pandora port
  ------------

  Ibisum


  ACIA & Pravetz disk support
  ---------------------------

  Iss


  CH376 support
  -------------

  Offset (cpc) & Jede


Thanks
======

Thanks to DBug and Twilighte for letting me distribute their demos and
games with Oricutron.

Thanks to DBug, Twilighte, Chema, kamelito, Yicker, JamesD, Algarbi, ibisum,
jede, thrust26 and everyone else for their help and feedback!



AVI export notes
================

The AVI export uses the MRLE codec. Your favourite player might not support
it, but MPlayer plays it, ffmpeg converts it and you can upload it directly
to youtube.

Note that the MRLE codec shows up some endian-issues on the Amiga OS4 port
of MPlayer, so it will sound crappy and have wrong colours until those bugs
are fixed :-(


Command line
============

You can specify certain options on the command line. All options have
both short and long versions. For example:

  -mblah

  or

  --machine blah

Is the same thing. Note that the short version doesn't have a space, but
the long version does.

Here are all the options:


  -m / --machine     = Specify machine type. Valid types are:

                       "atmos" or "a" for Oric atmos
                       "oric1" or "1" for Oric-1
                       "o16k" for Oric-1 16k
                       "telestrat" or "t" for Telestrat
                       "pravetz", "pravetz8d" or "p" for Pravetz 8D

  -d / --disk        = Specify a disk image to use in drive 0
  -t / --tape        = Specify a tape image to use
  -k / --drive       = Specify a disk drive controller. Valid types are:

                       "microdisc" or "m" for Microdisc
                       "jasmin" or "j" for Jasmin
                       "bd500" or "b" for ByteDrive BD-500
                       "pravetz" or "p" for Pravetz-8D FDC

  -s / --symbols     = Load symbols from a file
  -f / --fullscreen  = Run oricutron fullscreen
  -w / --window      = Run oricutron in a window
  -R / --rendermode  = Render mode. Valid modes are:

                       "soft" for software rendering
                       "opengl" for OpenGL

  -b / --debug       = Start oricutron in the debugger
  -r / --breakpoint  = Set a breakpoint (See NOTE2)
  -h / --help        = Print command line help and quit

  --turbotape on|off = Enable or disable turbotape
  --lightpen on|off  = Enable or disable lightpen
  --vsynchack on|off = Enable or disable VSync hack
  --scanlines on|off = Enable or disable scanline simulation

  --serial_address N = Set serial card base address to N (default is $31C)
                        where N is decimal or hexadecimal within the range of $31c..$3fc
                         (i.e. 796, 0x31c, $31C represent the same value)

  --serial <type>    = Set serial card back-end emulation:
                        'none' - no serial
                        'loopback' - for testing - all TX data is returned to RX
                        'modem[:port]' - emulates com port with attached modem,
                                         only minimal AT command set is supported and
                                         data is redirected to TCP. Default port is 23 (telnet)

                        'com:115200,8,N,1,<device>' - use real or virtual <device> on host as emulated ACIA.
                                         Baudrate, data bits, parity and stop bits can be set as needed
                                   ex.:  Windows: 'com:115200,8,N,1,COM1'
                                         Linux:   'com:19200,8,N,1,/dev/ttyS0'
                                                  'com:115200,8,N,1,/dev/ttyUSB0'

NOTE: If you are not sure what machine or drive type is required for a disk or
tape image, just pass the filename without any options and Oricutron will
try and autodetect for you.

NOTE2: List with many breakpoints can be loaded from command line. Use default switches -r or --breakpoint,
but instead of an address, specify filename prefixed with ':'. The file is plain text file, which contains
desired breakpoint-addresses - one per line using the same syntax as in the monitor. Breakpoints can be set
with absolute addresses or with symbols (loaded with command line switches -s or --symbols).

Examples:

oricutron tapes/tape_image.tap
oricutron disks/disk_image.dsk
oricutron --machine atmos --tape "tape files/foo.tap" --symbols "my files/symbols"
oricutron -m1 -tBUILD/foo.tap -sBUILD/symbols -b
oricutron --drive microdisc --disk demos/barbitoric.dsk --fullscreen
oricutron -ddemos/barbitoric.dsk -f
oricutron --turbotape off tapes/hobbit.tap
oricutron -s myproject.sym -r :myprojectbp.txt


Keys
====

  In emulator
  -----------

  F1       - Bring up the menu
  F2       - Go to debugger/monitor
  F3       - Reset button (NMI)
  F4       - Hard reset
  Shift+F4 - Jasmin reset
  F5       - Toggle FPS
  F6       - Toggle warp speed
  F7       - Save all modified disks
  Shift+F7 - Save all modified disks to new disk images
  F8       - Toggle fullscreen
  F9       - Save tape output
  F10      - Start/Stop AVI capture
  F11      - Copy text screen to clipboard (BeOS, Linux & Windows)
  F12      - Paste (BeOS, Linux & Windows)
  Help     - Show guide (Amiga, MorphOS and AROS)
  AltGr    - Additional modifier


  In menus
  --------

  Cursors   - Navigate
  Enter     - Perform option
  Backspace - Go back
  Escape    - Exit menus
  (or use the mouse)


  In Debugger/Monitor
  -------------------

  F1      - Go to the menu
  F2      - Return to the emulator
  F3      - Toggle console/debug output/memwatch
  F4      - Toggle VIA/AY/disk information
  F9      - Reset cycle count
  F10     - Step over code
  F11     - Step over code without tracing into
            subroutines.
  F12     - Skip instruction


  In the console:
  ---------------

  Up/Down - Command history


  In memwatch:
  ------------

  Up/Down           - Scroll (+shift for page up/down)
  Page Up/Page Down - Page up/down
  Hex digits        - Enter address
  S                 - Toggle split mode
  Tab               - Switch windows in split mode



Monitor instructions
====================

In the monitor, number arguments are decimal by default, or prefixed with $ for
hex or % for binary. Pretty much everything is output in hex.

In most places where you can enter a number or address, you can pass a CPU or
VIA register. (VIA registers are prefixed with V, e.g. VDDRA). Anywhere you can
pass an address, you can also use a symbol.

Commands:

  ?                     - Help
  a <addr>              - Assemble
  bc <bp id>            - Clear breakpoint
  bcm <bp id>           - Clear mem breakpoint
  bl                    - List breakpoints
  blm                   - List mem breakpoints
  bs <addr> [zc]        - Set breakpoint
  bsm <addr> [rwc]      - Set mem breakpoint
  bz                    - Zap breakpoints
  bzm                   - Zap mem breakpoints
  d <addr>              - Disassemble
  fd <addr> <end> <file>- Disassemble to file
  fw <addr> <len> <file>- Write mem to BIN file
  fr <addr> <file>      - Read BIN file to mem
  m <addr>              - Dump memory
  mm <addr> <value>     - Modify memory
  mw <addr>             - Memory watch at addr
  nl <file>             - Load snapshot
  ns <file>             - Save snapshot
  r <reg> <val>         - Set <reg> to <val>
  q, x or qm            - Quit monitor
  qe                    - Quit emulator
  sa <name> <addr>      - Add or move user symbol
  sk <name>             - Kill user symbol
  sc                    - Symbols not case-sensitive
  sC                    - Symbols case-sensitive
  sl <file>             - Load user symbols
  sx <file>             - Export user symbols
  sz                    - Zap user symbols



Breakpoints
===========

There are two types of breakpoint. "Normal" breakpoints trigger when the CPU
is about to execute an instruction at the breakpoint address. "Memory" breakpoints
trigger when the breakpoint address is accessed or modified.

Normal breakpoints can use 'z' and/or 'c' modifiers.
bs $0c00           <-- Break when the CPU is about to execute code at $0c00
bs $0c00 z         <-- Break when the CPU is about to execute code at $0c00
                       and set cycles counter to 0
bs $0c00 zc        <-- Set cycles counter to 0 and continues
bs $0c00 c         <-- Continues execution (i.e. disabled breakpoint)

The main purpose of these modifiers is to make cycle counting easier.
If symbols are loaded, they can be used instead of absolute addresses.

There are three ways a memory breakpoint can be triggered; when the CPU is about
to read the address (r), and the CPU is about to write the address (w), or after the
value at the address changes for any reason (c).

You specify which ways you'd like the breakpoint to trigger when you set the memory
breakpoint:

bsm $0c00 r        <-- Break when the CPU is about to read from $0c00
bsm $0c00 rw       <-- Break when the CPU is about to access $0c00
bsm $0c00 c        <-- Break after then contents of $0c00 change
bsm $0c00 rwc      <-- Break just before the CPU accesses $0c00, or just after it
                       changes for any reason.



International Keyboards under Linux and macOS
=============================================

There are lots of problems with some international keyboards under Linux
and macOS. The best way to cope with them is to install an UK or US
keyboard definition and to switch to it *before* starting oricutron.

Under macOS you can do that in the "System Preferences", "Keyboard", "Input
Sources". Click on the + and search for the UK or US keyboard.

Under Ubuntu you can do that in the System menu, select Preferences, and
then select Keyboard. In the Keyboard Preferences dialog, select the
Layouts tab, and click Add.

For a better solution look under "Visual Keyboard" down here.



Visual Keyboard
===============

Oricutron can display a visual keyboard which also adds a keyboard mapping redefinition feature.

It's accessible through a submenu called "Keyboard options".

In the submenu you can find:
- a toggle that shows/hides the visual keyboard (you can click on the keyboard keys to enter key presses/releases) ;
- a toggle that gets you in the key mapping definition mode (you can then click on a visual keyboard key ; press a real key on your keyboard and the mapping will work) ;
- a toggle that allows mod keys (ctrl, shift, funct) to be sticky (ie you first click on a key to press it and then either re-click it to release it or click on another key and it will generate a modded key press - e.g. a Ctrl-T instead of T - and then auto release the key) ;
- an option to save a keyboard mapping (.kma file) ;
- an option to load a keyboard mapping ;
- an option that resets the keyboard mapping to the default one.

You can also add the following in your oricutron.cfg to autoload a keyboard
mapping (here Test.kma in the keymap directory found in Oricutron's directory):

; automatically load a keyboard mapping file
autoload_keyboard_mapping = 'keymap/Test.kma'

Other options let you display the keyboard and activate sticky mod keys automatically:
show_keyboard = yes
sticky_mod_keys = yes



Serial card (ACIA) emulation
============================

Oricutron can emulate ACIA at address #31C (standard address for Telestrat).
The emulation works for Oric, Atmos, Telestrat and Pravetz and can be used
together with any disk type.

The emulated ACIA communicates with the out-side world through back-ends.
Back-ends can be configured from 'oricutron.cfg' or from command line
(see default 'oricutron.cfg' for usage).

Back-ends are:
- none      - disables ACIA support
- loopback  - every transmitted byte is returned to receive buffer (for testing purposes)
- com       - Oricutron uses any real or virtual COM port in the host machine and communicates with the hardware attached to this serial port
- modem     - unites ACIA with attached modem linked to internet with server and client sockets

In 'modem' mode are available folowing 'AT' commands:
AT          - returns 'OK'
ATZ         - initialize the modem
AT&F        - initialize the modem
ATS0=0      - disable auto answering (close sever socket)
ATS0=1      - enable auto answering (open sever socket and start listening on selected port (default is telnet port 23))
ATA         - same as 'ATS0=1'
ATS0?       - returns 'AUTOANSWER OFF' or 'AUTOANSWER ON' depend on current sever socket state
ATH0        - disconnect currently connected sockets
+++         - if connected switches to command mode
ATO         - returns from command mode to online
ATD ip:port - connects as client to ip:port. 'ip' can be any host name (ex.:localhost) or the real IP (ex.:127.0.0.1) on LAN or in Internet. ATDP and ATDT are alternative for compatibility.


CH376 card emulation
====================

Oricutron runs ch376 chip. This chip is able to read a sdcard and a usbkey
(and USB port). This chip handles FAT32. usbdrive/ folder is the CH376
emulation folder. It means that when we asked ch376 to read usbkey, it
reads in this folder. Please note, that read/write are emulated (see below for emulated ch376 command ). Please note that emulation runs only in
telestrat mode. Atmos can run this chip, but the rom had not been release yet (and the card with the rom).

Orix (http://orix.oric.org) works with this chip mainly. Don't modify ch376
emulation: contact Jede (jede@oric.org). Because this emulation is used
also in ACE emulator (cpc emulation). Offset and me are trying to keep the
same emulation. It's easier to work together than alone.

CH376 command emulated :
- CH376_CMD_GET_IC_VER
- CH376_CMD_CHECK_EXIST
- CH376_CMD_SET_USB_MODE
- CH376_CMD_GET_STATUS
- CH376_CMD_RD_USB_DATA0
- CH376_CMD_WR_REQ_DATA
- CH376_CMD_SET_FILE_NAME
- CH376_CMD_DISK_MOUNT
- CH376_CMD_FILE_OPEN
- CH376_CMD_FILE_ENUM_GO
- CH376_CMD_FILE_CREATE
- CH376_CMD_FILE_ERASE (not emulated under linux)
- CH376_CMD_FILE_CLOSE
- CH376_CMD_BYTE_LOCATE
- CH376_CMD_BYTE_READ
- CH376_CMD_BYTE_RD_GO
- CH376_CMD_BYTE_WRITE
- CH376_CMD_BYTE_WR_GO
- CH376_CMD_DISK_QUERY
- CH376_CMD_DIR_CREATE (not emulated under linux)
- CH376_CMD_DISK_RD_GO

Known bug :
Under windows, API's file does not send "." and ".." entries when we read the content on the folder. It's a problem because ch376 chip send these
entries, when we ask to this chip to read the content of a directory.

If someone wants to emulate CH376_CMD_DIR_CREATE and CH376_CMD_FILE_ERASE. It's easy to do : it justs need to copy WIN32 function and replace DeleteFile
 and CreateDir with rm() and mkdir(). But it's not done because we are not able to test it.

CH376 emulation added for :
CH376_CMD_FILE_ERASE suppress a file
CH376_CMD_DIR_CREATE create folder

Twilighte board emulation 
===============
The twilighte board is working on atmos (and Oric-1). It disables internal Oric ROM and add 64 banks (by bank switching).
It adds 32 banks of ROM (eeprom) and 32 banks of RAM (Static RAM saved with a battery).
EEprom can be programmed from Orix command line.
The board handles ch376 (sdcard/usbdrive/usb/hid controler).
And the board adds 2 joysticks ports.

The emulation, for instance, handles 32 ROM banks and 32 RAM banks. It emulates this bank switching. 

Board is not working with a microdisc. But jasmin does (mainly because jasmin does not replace rom at boot).

Anyway, on Telestrat, orix can start floppy disk because FDC is present. 

Not emulated parts :
* joysticks
* eeprom programming sequence
* loading ROM into RAM bank (which could emulate RAM saved with battery)
* save on disk any modification into RAM bank (whic could emulate RAM saved with battery)
* ch376 is not fully supported in the emulation mode. There is a lot of others functionnalities which are missing on ch376 emulation

To activate the plugin, you must activate twilighte_board option in oricutron.cfg

Update twilighte.cfg plugin in plugins/twilighte_card/twilighte.cfg if you want to load others roms.

For basic11 rom which handles joysticks and .tap load from sdcard or usbdrive, you have to download basic.tgz here : http://repo.orix.oric.org/dists/official/tgz/6502/
And you need to replace twilbankrom06 with the rom in basic11.tgz.


ROM patch files
===============

For detailed usage see included '.pch' files in roms subdirectory.

Additionaly unlimited number of binary patches can be added:

$XXXX:00112233445566778899AABBCCDDEEFF....
$YYYY:AA55AA55....
$ZZZZ:FF00FF00....

where XXXX,YYYY,ZZZZ - are hex addresses relative to ROM start address
(i.e. to set byte at C000 to 00 use: $0000:00)
