
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
  CLC
  ADC #$08  
  STA $0203
  LDA $0207
  CLC
  ADC #$08
  STA $0207
  LDA $020B
  SEC
  SBC #$08 
  STA $020B
  LDA $020F
  SEC
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
  SEC
  SBC #$08
  STA $0203
  LDA $0207
  SEC
  SBC #$08
  STA $0207
  LDA $020B
  CLC
  ADC #$08
  STA $020B
  LDA $020F
  CLC
  ADC #$08
  STA $020F
  
ReadRightDone:        ; handling this button is done
  JMP GameLoop