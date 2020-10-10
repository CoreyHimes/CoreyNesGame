  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring for horizontal games

;settings, uncomment or put them into your main program; the latter makes possible updates easier

FT_BASE_ADR		= $0300	;page in the RAM used for FT2 variables, should be $xx00
FT_TEMP			= $00	;3 bytes in zeropage used by the library as a scratchpad
FT_DPCM_OFF		= $c000	;$c000..$ffc0, 64-byte steps
FT_SFX_STREAMS	= 4		;number of sound effects played at once, 1..4

;
; FT_DPCM_ENABLE			;undefine to exclude all DMC code
; FT_SFX_ENABLE			;undefine to exclude all sound effects code
; FT_THREAD				;undefine if you are calling sound effects from the same thread as the sound update call

; FT_PAL_SUPPORT			;undefine to exclude PAL support
; FT_NTSC_SUPPORT			;undefine to exclude NTSC support



;internal defines
;;;;;;;;;;;;;;;

;; VARIABLES
  .rsset $0000
  
PLAYERXFIRST      = $05
PLAYERXSECOND     = $06
PLAYERXSPEED      = $07
PLAYERYLOW        = $08
PLAYERYHIGH       = $09
PLAYERISFORWARD   = $0A
MUSICISPLAYING    = $0B

  
;; CONSTANTS
GRAVITY    = $04

;;Common Memory Addresses
PPUCONTROL  = $2000
PPUMASK     = $2001
PPUSTATUS   = $2002
OAMADDR     = $2003
PPUADDR     = $2006
PPUDATA     = $2007
OAMDMA      = $4014
CONTROLLER1 = $4016
CONTROLLER2 = $4017
;;;;;;;;;;;;;;

    
  .bank 0
  .org $C000 
  
vblankwait:       ; First wait for vblank to make sure PPU is ready
  BIT PPUSTATUS
  BPL vblankwait
  RTS

RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs
  JSR vblankwait

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x    ;move all sprites off screen
  INX
  BNE clrmem

  JSR vblankwait
  
LoadPalettes:
  LDA PPUSTATUS
  LDA #$3F
  STA PPUADDR
  LDA #$00
  STA PPUADDR   
  
  LDX #$00
LoadPalettesLoop:
  LDA palette, x        ;load palette byte
  STA PPUDATA             ;write to PPU
  INX                   ;set index to next byte
  CPX #$20            
  BNE LoadPalettesLoop  ;if x = $20, 32 bytes copied, all done



  LDA #$80
  STA $0200        ; put sprite 0 in center ($80) of screen vert
  STA $0203        ; put sprite 0 in center ($80) of screen horiz
  LDA #$00
  STA $0201        ; tile number = 0
  STA $0202        ; color = 0, no flipping

  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0
  STA PPUCONTROL

  LDA #%00010000   ; enable sprites
  STA PPUMASK

LoadSprites:
  LDX #$00              ; start at 0

LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
              
              

  LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
  STA PPUCONTROL  

  LDA #%00010000   ; enable sprites
  STA PPUMASK

INITIALIZEPLAYER:
  ; lets zero out everything!
  LDA #0
  STA PLAYERXFIRST
  STA PLAYERXSECOND
  STA PLAYERXSPEED 
  STA PLAYERYLOW
  STA PLAYERYHIGH  
  STA PLAYERISFORWARD
  STA MUSICISPLAYING

INITIALIZELOOP:
  LDA #$01
  LDX #LOW(untitled_music_data)
  LDY #HIGH(untitled_music_data)
  JSR FamiToneInit

GameLoop:
  JSR vblankwait
LatchController:
  LDA #$01
  STA CONTROLLER1
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons
  
ReadA: 
  LDA CONTROLLER1       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
ReadADone:        ; handling this button is done
  

ReadB: 
  LDA CONTROLLER1       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
ReadBDone:        ; handling this button is done 

ReadSelect: 
  LDA CONTROLLER1       ; player 1 - Select
  AND #%00000001  ; only look at bit 0
  BEQ ReadSelectDone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
ReadSelectDone:        ; handling this button is done
  

ReadStart: 
  LDA CONTROLLER1       ; player 1 - Start
  AND #%00000001  ; only look at bit 0
  BEQ ReadStartDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
ReadStartDone:        ; handling this button is done

ReadUp: 
  LDA CONTROLLER1       ; player 1 - Up
  AND #%00000001  ; only look at bit 0
  BEQ ReadUpDone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
ReadUpDone:        ; handling this button is done
  

ReadDown: 
  LDA CONTROLLER1     ; player 1 - Down
  AND #%00000001  ; only look at bit 0
  BEQ ReadDownDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
ReadDownDone:        ; handling this button is done

InitializeLeft:
; check if we actually need to do anything
  LDA CONTROLLER1      
  AND #%00000001  
  BEQ ReadLeftDone  
  LDX #$00
  
ReadLeft: 
  LDA $0203, x       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0203, x       ; save sprite X position
  INX
  INX
  INX
  INX
  CPX #$10  ;
  BNE ReadLeft
  LDA PLAYERISFORWARD
  CMP #%01000000
  BNE FlipLeft
  JMP ReadLeftDone
 
FlipLeft:
  LDA #%01000000
  STA PLAYERISFORWARD
  LDX #$00
  JSR Flip
  
ReadLeftDone:        ; handling this button is done

InitializeRight:
; check if we actually need to do anything
  LDA CONTROLLER1      
  AND #%00000001  
  BEQ ReadRightDone  
  LDX #$00
  
ReadRight: 
  LDA $0203, x       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0203, x       ; save sprite X position
  INX
  INX
  INX
  INX
  CPX #$10
  BNE ReadRight
  LDA PLAYERISFORWARD
  CMP #%00000000
  BNE FlipRight
  JMP ReadRightDone

FlipRight:
  LDA #%00000000
  STA PLAYERISFORWARD
  LDX #$00
  JSR Flip
  
ReadRightDone:        ; handling this button is done  
  
PlayMusic:
  LDA MUSICISPLAYING	
  BNE MusicDone
  LDA #$00
  JSR FamiToneMusicPlay
  LDA #$01
  STA MUSICISPLAYING
MusicDone:
  JMP GameLoop

NMI:
  LDA #$00
  STA $2003  ; set the low byte (00) of the RAM address
  LDA #$02
  STA OAMDMA  ; set the high byte (02) of the RAM address, start the transfer
  JSR FamiToneUpdate
  RTI

Flip:
  ;Flips Position
  LDA $0203, x
  PHA
  LDA $0207, x
  STA $0203, x
  PLA
  STA $0207, x
  
  ;Flips Sprites
  LDA PLAYERISFORWARD
  STA $0202, x
  STA $0206, x
  
  ;Loop it up
  TXA
  CLC
  ADC #$08
  TAX
  CPX #$10 ;This should eventually be based on the y register so I can flip all sprites
  BNE Flip
  
  RTS
music:
  .include "famitone2.asm"
  .include "untitled.asm"
;;;;;;;;;;;;;;    
  
  .bank 1
  .org $E000
palette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$06,$27,$14,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1 
  .db $88, $34, $00, $80   ;sprite 2  
  .db $88, $35, $00, $88   ;sprite 3

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial

;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1
