  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring for horizontal games
  

;;;;;;;;;;;;;;;

;; VARIABLES
  .rsset $0000
  
PLAYERXFIRST      = $02
PLAYERXSECOND     = $03
PLAYERXSPEED      = $04
PLAYERYLOW        = $05
PLAYERYHIGH       = $06
PLAYERISFORWARD   = $07

  
;; CONSTANTS
GRAVITY    = $01

;;Common Memory Addresses
PPUCONTROL  = $2000
PPUMASK     = $2001
PPUSTATUS   = $2002
OAMADDR     = $2003
PPUADDR     = $2006
PPUDATA     = $2007
OAMDMA
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
  JSR INITIALIZEPLAYER
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
  ;;the sad writing 16 bit life
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
  RTS

Forever:
  JMP Forever     ;jump forever so the 6502 is never bored

NMI:
  LDA #$00
  STA $2003  ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014  ; set the high byte (02) of the RAM address, start the transfer
  
  

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
  
FaceLeftCheck:
  LDA PLAYERISFORWARD
  CMP #%01000000
  BEQ ReadLeftDone ;already facing left, no need to flip sprites
  LDA #%01000000  ; makes the sprite face left
  STA PLAYERISFORWARD
  STA $0202
  STA $0206
  STA $020A
  STA $020E
  LDA $0203
  ADC #$08
  STA $0203
  LDA $0207
  ADC #$08
  STA $0207
  LDA $020B
  SBC #$07
  STA $020B
  LDA $020F
  SBC #$08
  STA $020F
  
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
  
FaceRightCheck:
  LDA PLAYERISFORWARD
  CMP #%00000000
  BEQ ReadRightDone ; already facing right, no need to flip sprites 
  LDA #%00000000 ; makes the sprite face left
  STA PLAYERISFORWARD
  STA $0202
  STA $0206
  STA $020A
  STA $020E
  LDA $0203
  SBC #$08
  STA $0203
  LDA $0207
  SBC #$08
  STA $0207
  LDA $020B
  ADC #$07
  STA $020B
  LDA $020F
  ADC #$08
  STA $020F
  
ReadRightDone:        ; handling this button is done


  RTI        ; return from interrupt
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000
palette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$06,$27,$14,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $88, $34, $00, $80   ;sprite 1
  .db $80, $33, $00, $88   ;sprite 2  
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