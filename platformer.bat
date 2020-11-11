ca65 platformer.asm -o platformer.o --debug-info
ld65 platformer.o -o platformer.nes -t nes --dbgfile platformer.dbg
pause