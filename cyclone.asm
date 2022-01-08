;i/o
Z80         = $a00000
VERSION     = $a10001
PORT1DATA   = $a10003
PORT2DATA   = $a10005
EXPDATA     = $a10007
PORT1CTRL   = $a10009
PORT2CTRL   = $a1000b
EXPCTRL     = $a1000d
PORT1TxDATA = $a1000f
PORT1RxDATA = $a10011
PORT1SCTRL  = $a10013
PORT2TxDATA = $a10015
PORT2RxDATA = $a10017
PORT2SCTRL  = $a10019
EXPTxDATA   = $a1001b
EXPRxDATA   = $a1001d
EXPSCTRL    = $a1001f
MEMMODE     = $a11000
Z80BUSREQ   = $a11100
Z80RESET    = $a11200
TMSSCTRL    = $a14000
VDPDATA     = $c00000
VDPCTRL     = $c00004
HVCOUNTER   = $c00008
PSG         = $c00011



;vdpctrl transfer id codes
              ;10......................5432....
VRAM_WRITE  = %01000000000000000000000000000000
CRAM_WRITE  = %11000000000000000000000000000000
VSRAM_WRITE = %01000000000000000000000000010000
VRAM_DMA    = %01000000000000000000000010000000
CRAM_DMA    = %11000000000000000000000010000000
VSRAM_DMA   = %01000000000000000000000010010000
VRAM_READ   = %00000000000000000000000000000000
CRAM_READ   = %00000000000000000000000000100000
VSRAM_READ  = %00000000000000000000000000010000

            ;params: CONSTANT address, transfer type
SetVDPAddr  macro
            ;if (\1) < 0 || (\1) > $ffff
            ;    fail "VRAM addresses must be from $0000-$ffff"
            ;endif
            if NARG != 2
                fail "Bad argument count"
            endif
            move.l #(((\1) & $3fff) << 16) | (((\1) & $3c000) >> 14) | (\2), VDPCTRL
            endm
            
            ;params: CONSTANT address, transfer type, addressing mode
SetVDPAddrA macro
            ;if (\1) < 0 || (\1) > $ffff
            ;    fail "VRAM addresses must be from $0000-$ffff"
            ;endif
            if NARG != 3
                fail "Bad argument count"
            endif
            move.l #(((\1) & $3fff) << 16) | (((\1) & $3c000) >> 14) | (\2), \3
            endm
            
            ;params: transfer type, data register
ConvVDPAddr macro
            if NARG != 2
                fail "Bad argument count"
            endif
            ;andi.l #$ffff, \2
            lsl.l #2, \2
            lsr.w #2, \2
            swap \2
            if (\1)
            ori.l #(\1), \2
            endif
            endm
            
            ;params: constant value, register number
SetVDPReg   macro
            if NARG != 2
                fail "Bad argument count"
            endif
            move.w #$8000 | ((\2) << 8) | (\1), VDPCTRL
            endm
            ;params: constant value, register number, addressing mode
SetVDPRegA  macro
            if NARG != 3
                fail "Bad argument count"
            endif
            move.w #$8000 | ((\2) << 8) | (\1), \3
            endm
            
            ;this is for setting two vdp registers at once
            ;params: src1,dest1, src2,dest2
SetVDPRegs  macro
            if NARG != 4
                fail "Bad argument count"
            endif
            move.l #(($8000 | ((\2) << 8) | (\1)) << 16) | ($8000 | (\4 << 8) | (\3)), VDPCTRL
            endm
            ;params: src1,dest1, src2,dest2, addressing mode
SetVDPRegsA macro
            if NARG != 5
                fail "Bad argument count"
            endif
            move.l #(($8000 | ((\2) << 8) | (\1)) << 16) | ($8000 | (\4 << 8) | (\3)), \5
            endm







    
    code
    
    ;this flag disables $bxxxxx accesses which freeze bizhawk
;EMU_COMPATIBLE = 1
    
    ;global vars
    pushsection
    offset $ffff8000
vblank_mode_ptr
    dl 0
vblank_flag
    db 0
    
rawjoy
    db 0
joy db 0

a_hold_timer
    db 0
    popsection
    
    
    
    
    
    org 0
    dl $ffff8000
    dl reset
    
    dl ex_bus_error
    dl ex_address_error
    dl ex_illegal_instruction
    dl ex_division_by_zero
    dl ex_chk
    dl ex_trapv
    dl ex_privilege_violation
    dl ex_trace
    dl ex_unimplemented_a
    dl ex_unimplemented_f
    org $3c
    dl ex_uninitialized
    org $60
    dl ex_i0
    dl ex_i1
    dl ex_i2
    dl ex_i3
    dl ex_i4
    dl ex_i5
    dl vblank ;ex_i6
    dl ex_i7
    dl ex_trap0
    dl ex_trap1
    dl ex_trap2
    dl ex_trap3
    dl ex_trap4
    dl ex_trap5
    dl ex_trap6
    dl ex_trap7
    dl ex_trap8
    dl ex_trap9
    dl ex_trap10
    dl ex_trap11
    dl ex_trap12
    dl ex_trap13
    dl ex_trap14
    dl ex_trap15
    
    
    org $100
sega:
    db "SEGA GENESIS    "
    db "KARMIC  2022.JAN"
    ifnd EMU_COMPATIBLE
      db "CYCLONE                                         "
      db "CYCLONE                                         "
    else
      db "CYCLONE  (EMULATOR COMPATIBILITY MODE)          "
      db "CYCLONE  (EMULATOR COMPATIBILITY MODE)          "
    endif
    db "TS 00000000-00"
    dw $DEAD
    db "JD              "
    dl 0,rom_end-1
    dl $ff0000,$ffffff
    db "            "
    db "            "
    db "karmic128.neocities.org/sgflashback     "
    db "U               "
    
    
    ;system functions
BGABASE = $a000
BGBBASE = $c000
WINDOWBASE = $e000
SATBASE = $f000
HSCROLLBASE = $f400

SCREENWIDTH = 64
SCREENWIDTH_SHIFT = 7

SCREENHEIGHT = 32
    
    
init_vdp_regs:
    lea VDPCTRL,a0
    SetVDPRegA 0,$18, (a0)  ;disable cyclone mode
    SetVDPRegsA 4,0, $24,1, (a0)
    SetVDPRegsA (BGABASE>>10),2, (BGBBASE>>13),4, (a0)
    SetVDPRegsA (WINDOWBASE>>10),3, (SATBASE>>9),5, (a0)
    SetVDPRegsA 0,6, 0,7, (a0)
    SetVDPRegsA 0,8, 0,9, (a0)
    SetVDPRegsA 0,11, $81,12, (a0)
    SetVDPRegsA (HSCROLLBASE>>10),13, 0,14, (a0)
    SetVDPRegsA 2,15, $01,16, (a0)
    SetVDPRegsA 0,17, 0,18, (a0)
    rts
    
    
    
    
    
init_vdp_transfer:
    lea VDPDATA,a1
    lea 4(a1),a0
    rts
    
    
wait_vdp_dma:
    btst.b #1,1(a0)
    bne wait_vdp_dma
    rts
    
    
    
    
clear_vram:
    bsr init_vdp_transfer
    SetVDPRegA $00,$18, (a0)
    
    moveq #0,d0
    
    
    ;doing it twice is actually necessary otherwise the background will still
    ;be grey when exiting the vram size test
    SetVDPAddrA 0,CRAM_WRITE,(a0)
    move.w #($80/4)-1,d7
.cram_1:
    move.l d0,(a1)
    dbra d7,.cram_1
    
    
    SetVDPRegA $80,$18, (a0) ;enable access to extra ram
    
    SetVDPAddrA 0,VRAM_WRITE,(a0)
    move.w #($1ff74/4)-1,d7
.vram:
    move.l d0,(a1)
    dbra d7,.vram
    
    
    SetVDPAddrA 0,CRAM_WRITE,(a0)
    move.w #($800/4)-1,d7
.cram_2:
    move.l d0,(a1)
    dbra d7,.cram_2
    
    SetVDPRegA $00,$18, (a0)
    
    
    SetVDPAddrA 0,VSRAM_WRITE,(a0)
    moveq #($50/4)-1,d7
.vsram:
    move.l d0,(a1)
    dbra d7,.vsram
    rts
    
    
    
    
    
init_cram:
    bsr init_vdp_transfer
    
    SetVDPAddrA 0,CRAM_WRITE, (a0)
    
    moveq #0,d0
.l1 move.w d0,(a1)
    addi.w #$1111,d0
    bcc .l1
    
    moveq #0,d0
.l2 move.w d0,(a1)
    addi.w #$1001,d0
    bcc .l2
    
    moveq #0,d0
.l3 move.w d0,(a1)
    addi.w #$1010,d0
    bcc .l3
    
    moveq #0,d0
.l4 move.w d0,(a1)
    addi.w #$1100,d0
    bcc .l4
    
    rts
    
    
    
    
init_cram_cyclone:
    bsr init_vdp_transfer
    SetVDPRegA $80,$18, (a0)
    SetVDPAddrA 0,CRAM_WRITE, (a0)
    
    
    moveq #0,d0
.l1 move.l d0,(a1)
    addi.l #$04040404,d0
    bcc .l1
    
    moveq #0,d0
    moveq #($100/4)-1,d1
.l2 move.l d0,(a1)
    addi.l #$00040000,d0
    dbra d1,.l2
    
    moveq #0,d0
    moveq #($100/4)-1,d1
.l3 move.l d0,(a1)
    addi.l #$04000000,d0
    dbra d1,.l3
    
    moveq #0,d0
    moveq #($100/4)-1,d1
.l4 move.l d0,(a1)
    addi.l #$00000004,d0
    dbra d1,.l4
    
    moveq #0,d0
    moveq #($100/4)-1,d1
.l5 move.l d0,(a1)
    addi.l #$04000004,d0
    dbra d1,.l5
    
    moveq #0,d0
    moveq #($100/4)-1,d1
.l6 move.l d0,(a1)
    addi.l #$04040000,d0
    dbra d1,.l6
    
    moveq #0,d0
    moveq #($100/4)-1,d1
.l7 move.l d0,(a1)
    addi.l #$00040004,d0
    dbra d1,.l7
    
    move.l #$fcfc00fc,d0
    moveq #($100/4)-1,d1
.l8 move.l d0,(a1)
    subi.l #$02020002,d0
    dbra d1,.l8
    
    
    
    rts
    
    
    
    
upload_font:
    bsr init_vdp_transfer
    
    SetVDPAddrA 0,VRAM_WRITE, (a0)
    
    move.w #($80*8)-1,d7
    lea font_data,a2
    
.byteloop:
    move.b (a2)+,d0
    moveq #0,d1
    moveq #8-1,d6
.pixloop:
    lsl.l #4,d1
    add.b d0,d0
    bcc .skippix
    ori.b #$0f,d1
.skippix:
    
    dbra d6,.pixloop
    
    move.l d1,(a1)
    dbra d7,.byteloop
    
    rts
    
    
    
upload_font_cyclone:
    bsr init_vdp_transfer
    SetVDPRegA $80,$18, (a0)
    SetVDPAddrA 0,VRAM_WRITE, (a0)
    
    
    move.w #($80*8)-1,d7
    lea font_data,a2
    lea .color_tbl,a3
    
.byteloop:
    move.b (a2)+,d0
    
    moveq #0,d1 ;output least significant word
    moveq #0,d2 ;output most significant long
    move.b d7,d3
    andi.b #7,d3
    move.b (a3,d3),d3 ;pixel or-value lsw
    moveq #0,d4 ;pixel or-value msl
    
    moveq #8-1,d6
.pixloop:
    add.b d0,d0
    bcc .skippix
    or.w d3,d1
    or.l d4,d2
.skippix:
    
    rept 6
      lsl.w #1,d3
      roxl.l #1,d4
    endr
    
    dbra d6,.pixloop
    
    move.w d1,(a1)
    swap d2
    move.l d2,(a1)
    dbra d7,.byteloop
    
    rts
    
    ;give 6bpp text some color bars
.color_tbl:
    db $20,$27,$30,$37
    db $3f,$37,$2f,$27
    
    
    
    ; in: A2=string ptr, D0=VRAM ptr (0 to skip), D1=nametable OR value, D2+ =format parameters
    ;out: A0=VDPCTRL, A1=VDPDATA, A2=string end ptr
FMT_CHAR = $80
FMT_NIB = $81
FMT_BYTE = $82
FMT_WORD = $83
FMT_24 = $84
FMT_LONG = $85
FMT_DEC = $86
output_string_normal:
    moveq #0,d1
output_string:
    movem.l d0/d2-d7,-(a7)
    
    bsr init_vdp_transfer
    
    tst.w d0
    beq .skip_addr
    ConvVDPAddr VRAM_WRITE,d0
    move.l d0,(a0)
.skip_addr:
    
.loop:
    moveq #0,d0
    move.b (a2)+,d0
    bmi .format
    beq .end
.normal:
    or.w d1,d0
    move.w d0,(a1)
    bra .loop
.format:
    cmpi.b #FMT_NIB,d0
    beq .nib
    cmpi.b #FMT_BYTE,d0
    beq .byte
    cmpi.b #FMT_WORD,d0
    beq .word
    cmpi.b #FMT_24,d0
    beq ._24
    cmpi.b #FMT_LONG,d0
    beq .long
    cmpi.b #FMT_DEC,d0
    beq .dec
    bsr .get_next ;char
    bra .normal
    
.nib:
    bsr .get_next
    bsr output_nib
    bra .loop
.byte:
    bsr .get_next
    bsr output_byte
    bra .loop
.word:
    bsr .get_next
    bsr output_word
    bra .loop
._24:
    bsr .get_next
    bsr output_24
    bra .loop
.long:
    bsr .get_next
    bsr output_long
    bra .loop
.dec:
    bsr .get_next
    bsr output_dec
    bra .loop
    
.end:
    movem.l (a7)+, d0/d2-d7
    rts
    
    
.get_next:
    move.l d2,d0
    move.l d3,d2
    move.l d4,d3
    move.l d5,d4
    move.l d6,d5
    move.l d7,d6
    rts
    
    
    ;all of these take the value in D0 and nametable OR in D1
output_byte:
    ror.l #4,d0
    bsr output_nib
    rol.l #4,d0
    bra output_nib
    
    
output_word:
    swap d0
    bra output_upper_word
    
    
output_24:
    rol.l #8,d0
    bsr output_upper_byte
    bra output_upper_word
    
    
output_long:
    bsr output_upper_word
output_upper_word:
    bsr output_upper_byte
output_upper_byte:
    rol.l #4,d0
    bsr output_nib
    rol.l #4,d0
    ;... fall through ...
output_nib:
    move.l d0,-(a7)
    lea output_num_tbl,a0
    andi.l #$0f,d0
    move.b (a0,d0),d0
    or.w d1,d0
    move.w d0,(a1)
    lea 4(a1),a0
    move.l (a7)+,d0
    rts
    
    
    
    ;(doesn't support anything larger than 99999)
output_dec:
    movem.l d2-d4/a2, -(a7)
    
    moveq #0,d2 ;nonzero flag
    moveq #0,d3
    move.w #10000,d3 ;current divisor
    lea output_num_tbl, a2
    
.loop:
    divu.w d3,d0
    
    ;quotient is the digit to print
    tst.b d2 ;if a nonzero digit has been printed before, always print it
    bne .always
    cmpi.w #1,d3 ;or if the divisor is 1 (this is the last digit)
    beq .always
    tst.w d0
    beq .skip
.always:
    moveq #1,d2
    ;output num in low-word of d0
    moveq #0,d4
    move.b d0,d4
    move.b (a2,d4), d4
    or.w d1,d4
    move.w d4,(a1)
.skip:
    clr.w d0 ;remainder is the new dividend
    swap d0
    
    divu.w #10,d3 ;end?
    bne .loop
    
    movem.l (a7)+, d2-d4/a2
    rts
    
    
output_num_tbl:
    db "0123456789ABCDEF"
    
    
    
    
full_init_vdp:
    bsr init_vdp_regs
    bsr clear_vram
    bsr init_cram
    bra upload_font
    
    
    
    
    ;exceptions
ex_bus_error:
    move.l #em_bus_error,-(a7)
    bra exception_main
ex_address_error:
    move.l #em_address_error,-(a7)
    bra exception_main
ex_illegal_instruction:
    move.l #em_illegal_instruction,-(a7)
    bra exception_main
ex_division_by_zero:
    move.l #em_division_by_zero,-(a7)
    bra exception_main
ex_chk:
    move.l #em_chk,-(a7)
    bra exception_main
ex_trapv:
    move.l #em_trapv,-(a7)
    bra exception_main
ex_privilege_violation:
    move.l #em_privilege_violation,-(a7)
    bra exception_main
ex_trace:
    move.l #em_trace,-(a7)
    bra exception_main
ex_unimplemented_a:
    move.l #em_unimplemented_a,-(a7)
    bra exception_main
ex_unimplemented_f:
    move.l #em_unimplemented_f,-(a7)
    bra exception_main
ex_uninitialized:
    move.l #em_uninitialized,-(a7)
    bra exception_main
ex_i0:
    move.l #em_spurious_interrupt,-(a7)
    bra exception_main
    ;abuse the upper byte of the vector for the interrupt level
ex_i1:
    move.l #em_interrupt | (1 << 24),-(a7)
    bra exception_main
ex_i2:
    move.l #em_interrupt | (2 << 24),-(a7)
    bra exception_main
ex_i3:
    move.l #em_interrupt | (3 << 24),-(a7)
    bra exception_main
ex_i4:
    move.l #em_interrupt | (4 << 24),-(a7)
    bra exception_main
ex_i5:
    move.l #em_interrupt | (5 << 24),-(a7)
    bra exception_main
ex_i6:
    move.l #em_interrupt | (6 << 24),-(a7)
    bra exception_main
ex_i7:
    move.l #em_interrupt | (7 << 24),-(a7)
    bra exception_main
ex_trap0:
    move.l #em_trap | (0 << 24),-(a7)
    bra exception_main
ex_trap1:
    move.l #em_trap | (1 << 24),-(a7)
    bra exception_main
ex_trap2:
    move.l #em_trap | (2 << 24),-(a7)
    bra exception_main
ex_trap3:
    move.l #em_trap | (3 << 24),-(a7)
    bra exception_main
ex_trap4:
    move.l #em_trap | (4 << 24),-(a7)
    bra exception_main
ex_trap5:
    move.l #em_trap | (5 << 24),-(a7)
    bra exception_main
ex_trap6:
    move.l #em_trap | (6 << 24),-(a7)
    bra exception_main
ex_trap7:
    move.l #em_trap | (7 << 24),-(a7)
    bra exception_main
ex_trap8:
    move.l #em_trap | (8 << 24),-(a7)
    bra exception_main
ex_trap9:
    move.l #em_trap | (9 << 24),-(a7)
    bra exception_main
ex_trap10:
    move.l #em_trap | (10 << 24),-(a7)
    bra exception_main
ex_trap11:
    move.l #em_trap | (11 << 24),-(a7)
    bra exception_main
ex_trap12:
    move.l #em_trap | (12 << 24),-(a7)
    bra exception_main
ex_trap13:
    move.l #em_trap | (13 << 24),-(a7)
    bra exception_main
ex_trap14:
    move.l #em_trap | (14 << 24),-(a7)
    bra exception_main
ex_trap15:
    move.l #em_trap | (15 << 24),-(a7)
    bra exception_main
    
    
exception_main:
    addq.b #1,vblank_flag
    movem.l d0-d7/a0-a6,-(a7)
    
    bsr full_init_vdp
    
    
    ;data registers
    move.l #BGABASE+(SCREENWIDTH*2*9)+8,d0
    moveq #'D',d2
    moveq #'0',d3
.dloop:
    lea .str_reg,a2
    move.l (a7)+,d4
    bsr output_string_normal
    addi.l #SCREENWIDTH*2,d0
    addq.b #1,d3
    cmpi.b #'8',d3
    blo .dloop
    
    
    
    ;address registers
    addi.l #SCREENWIDTH*2,d0
    moveq #'A',d2
    moveq #'0',d3
.aloop:
    lea .str_reg,a2
    move.l (a7)+,d4
    bsr output_string_normal
    addi.l #SCREENWIDTH*2,d0
    addq.b #1,d3
    cmpi.b #'7',d3
    blo .aloop
    
    
    ;exception message
    move.l (a7)+,a6
    move.l a6,d0
    move.l a6,d1
    rol.l #8,d0
    
    andi.l #$ffffff,d1
    movea.l d1,a2
    
    moveq #0,d2
    move.b d0,d2
    move.l #(BGABASE+(SCREENWIDTH*2*2)+4),d0
    move.w #$2000,d1
    bsr output_string
    
    
    ;special bus/address error information
    cmpa.l #em_bus_error,a6
    beq .dospecial
    cmpa.l #em_address_error,a6
    bne .skipspecial
.dospecial:
    ;first word
    move.w (a7)+, d7
    
    ;w/r
    move.b #'W',d2
    btst #4,d7
    beq .noread
    move.b #'R',d2
.noread:
    
    ;i/n
    move.b #'I',d3
    btst #3,d7
    beq .nonot
    move.b #'N',d3
.nonot:
    
    ;fn
    moveq #0,d4
    move.b d7,d4
    andi.b #7,d4
    
    ;address
    move.l (a7)+,d5
    
    ;instruction register
    move.w (a7)+,d6
    
    ;print
    move.l #BGABASE+(SCREENWIDTH*2*3)+24, d0
    lea .str_address,a2
    bsr output_string_normal
    
.skipspecial:
    
    
    
    ;status register
    move.l #BGABASE+(SCREENWIDTH*2*6)+6,d0
    move.w (a7)+,d2
    lea .str_sr,a2
    bsr output_string_normal
    
    ;program counter
    move.l #BGABASE+(SCREENWIDTH*2*5)+6,d0
    moveq #'P',d2
    moveq #'C',d3
    move.l (a7)+,d4
    lea .str_reg,a2
    bsr output_string_normal
    
    
    ;stack pointer
    move.l #BGABASE+(SCREENWIDTH*2*25)+8,d0
    moveq #'A',d2
    moveq #'7',d3
    move.l a7,d4
    lea .str_reg,a2
    bsr output_string_normal
    
    
    ;end
    move.l #mode_exception, vblank_mode_ptr
    move.b #0,vblank_flag
    bra *
    
    
.str_sr:
    db "SR:$",FMT_WORD,0
.str_reg:
    db FMT_CHAR,FMT_CHAR,":$",FMT_LONG,0
.str_address:
    db FMT_CHAR," ",FMT_CHAR," ","Fn:",FMT_NIB," $",FMT_LONG," IR:$",FMT_WORD,0
    
    
    
em_bus_error:
    db "Bus error",0
em_address_error:
    db "Address error",0
em_illegal_instruction:
    db "Illegal instruction",0
em_division_by_zero:
    db "Division by zero",0
em_chk:
    db "CHK out of bounds",0
em_trapv:
    db "TRAPV overflow",0
em_privilege_violation:
    db "Privilege violation",0
em_trace:
    db "Trace",0
em_unimplemented_a:
    db "Unimplemented instruction A",0
em_unimplemented_f:
    db "Unimplemented instruction F",0
em_uninitialized:
    db "Uninitialized interrupt",0
em_spurious_interrupt:
    db "Spurious interrupt",0
em_interrupt:
    db "Interrupt level ",FMT_DEC,0
em_trap:
    db "TRAP #",FMT_DEC,0
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    align 1
reset:
    move #$2700,sr
    move.l (0),a7
    reset
    
    lea $a10000,a0
    move.b VERSION-$a10000(a0), d0
    andi.b #$0f, d0
    beq .skiptmss
    move.l sega, TMSSCTRL-$a10000(a0)
.skiptmss:
    
    move.b #$40,d0
    move.b d0,PORT1CTRL-$a10000(a0)
    move.b d0,PORT1DATA-$a10000(a0)
    move.b d0,PORT2CTRL-$a10000(a0)
    move.b d0,PORT2DATA-$a10000(a0)
    
    lea PSG,a0
    move.b #$9f,(a0)
    move.b #$bf,(a0)
    move.b #$df,(a0)
    move.b #$ff,(a0)
    
    moveq #0,d0
    move.l d0,d1
    move.l d0,d2
    move.l d0,d3
    move.l d0,a0
    move.l d0,a1
    move.l d0,a2
    move.l d0,a3
    move.l d0,a4
    move.w #($10000/8/4)-1,d4
.memclr:
    movem.l d0-d3/a0-a3,-(a4)
    dbra d4,.memclr
    
    
    move.b #$ff,rawjoy
    
    move.w #$1000, mode_vram_test\.base_address
    
    lea mode_tile_test\.palette,a0
    moveq #0,d0
.palclr:
    move.l d0,(a0)+
    addi.l #$04040404,d0
    bcc .palclr
    move.w #$1050, mode_tile_test\.tile_base
    move.w #$0100, mode_tile_test\.palette_base
    move.w #($1050/$30) | $2000, mode_tile_test\.tile
    
    move.b #$c0, mode_bitmap_overlay_test\._18
    
    
    addq.b #1,vblank_flag
    bsr init_vdp_regs
    move #$2000,sr
    move.l #mode_main_menu,vblank_mode_ptr
    subq.b #1,vblank_flag
    
    bra *
    
    
    
BTN_S = 7
BTN_A = 6
BTN_C = 5
BTN_B = 4
BTN_R = 3
BTN_L = 2
BTN_D = 1
BTN_U = 0
    
vblank:
    tst.b vblank_flag
    bne .skip
    
    movem.l d0-d7/a0-a6,-(a7)
    addq.b #1,vblank_flag
    move #$2000,sr
    
    ;get joystick
    lea rawjoy,a1
    
    lea PORT1DATA,a0
    move.b #$40,(a0)
    move.b (a1),d2
    not.b d2
    nop
    nop
    move.b (a0),d0
    
    move.b #$00,(a0)
    and.b #$3f,d0
    nop
    nop
    move.b (a0),d1
    
    and.b #$30,d1
    lsl.b #2,d1
    or.b d1,d0
    not.b d0
    move.b d0,(a1)
    
    lea a_hold_timer,a0
    btst #BTN_A,d0
    beq .clearholda
    cmpi.b #$ff,(a0)
    beq .skipholda
    addq.b #1,(a0)
    bra .skipholda
.clearholda:
    move.b #0,(a0)
.skipholda:
    
    and.b d0,d2
    move.b d2,1(a1)
    
    ;start goes back to menu
    movea.l vblank_mode_ptr,a0
    cmpa.l #mode_main_menu\.main,a0
    beq .nostart
    cmpa.l #mode_exception,a0
    beq .nostart
    btst #BTN_S, d2
    beq .execute
    movea.l #mode_main_menu,a0
.nostart:
    ;execute
.execute:
    jsr (a0)
    
    subq.b #1,vblank_flag
    movem.l (a7)+, d0-d7/a0-a6
.skip:
    rte
    
    
    
    ; in: d0=value, d1=joy, d2=add-shift-count/4
    ;out: d0=new, d1=no change/add/sub flag  (zero flag indicates change)
do_inc_dec:
    moveq #1,d3 ;get add value
    lsl.l #2,d2
    lsl.l d2,d3
    
    btst #BTN_C,d1
    bne .inc
    btst #BTN_B,d1
    bne .dec
    moveq #0,d1
    rts
.inc:
    add.l d3,d0
    moveq #1,d1
    rts
.dec:
    sub.l d3,d0
    moveq #2,d1
    rts
    
    
    
    
    
mode_exception:
    SetVDPReg $64,1
    bra *
    
mode_screenon:
    SetVDPReg $64,1
    rts
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    align 1
mode_main_menu:

    pushsection
    offset
    ;align 4
.selected:
    db 0
    popsection
    
    bsr full_init_vdp
    
    ifd EMU_COMPATIBLE
      move.l #BGABASE+(SCREENWIDTH*2*3)+4,d0
      move.w #$2000,d1
      lea .emutext,a2
      bsr output_string
    endif
    
    move.l #BGABASE+(SCREENWIDTH*2*2)+16,d0
    lea .titletext,a2
    bsr output_string_normal
    
    move.l #BGABASE+(SCREENWIDTH*2*25)+16,d0
    lea .helptext1,a2
    bsr output_string_normal
    move.l #BGABASE+(SCREENWIDTH*2*26)+10,d0
    bsr output_string_normal
    
    move.l #BGABASE+(SCREENWIDTH*2*22)+8,d0
    lea .authtext1,a2
    bsr output_string_normal
    move.l #BGABASE+(SCREENWIDTH*2*23)+6,d0
    bsr output_string_normal
    
    
    moveq #0,d0
.init_opt_loop:
    bsr .print_opt_normal
    addi.l #1,d0
    cmpi.b #.OPT_COUNT,d0
    blo .init_opt_loop
    
    
    
    move.l #.main,vblank_mode_ptr
    rts
    
    
.main:
    SetVDPReg $64,1
    
    ;uncolor previous opt
    moveq #0,d0
    move.b .selected,d0
    bsr .print_opt_normal
    
    ;navigate
    move.b joy,d1
    btst #BTN_D,d1
    bne .nav_d
    btst #BTN_U,d1
    beq .skip_nav
    tst.b d0
    beq .skip_nav
    subi.b #1,d0
    bra .skip_nav
.nav_d:
    cmpi.b #.OPT_COUNT-1,d0
    bhs .skip_nav
    addi.b #1,d0
.skip_nav:
    move.b d0,.selected
    
    ;color new opt
    move.w #$4000,d1
    bsr .print_opt
    
    
    
    cmpi.b #$9a,a_hold_timer
    bne .no_secret
    cmpi.b #(1<<BTN_A) | (1<<BTN_U) | (1<<BTN_L),rawjoy
    bne .no_secret
    move.l #mode_secret,vblank_mode_ptr
.no_secret:

    
    ;select?
    btst.b #BTN_C,joy
    beq .noselect
    lea .mode_tbl,a0
    lsl.l #2,d0
    move.l (a0,d0),vblank_mode_ptr
.noselect:
    
    
    rts
    
    
.print_opt_normal:
    moveq #0,d1
.print_opt:
    move.l d0,-(a7)
    
    move.l d0,d2
    lsl.l #SCREENWIDTH_SHIFT,d0
    addi.w #BGABASE+(SCREENWIDTH*2*5)+10,d0
    
    lea .opt_tbl,a2
    lsl.l #2,d2
    movea.l (a2,d2),a2
    
    bsr output_string
    
    move.l (a7)+,d0
    rts
    

.OPT_COUNT = 8
.opt_tbl:
    dl .str_0
    dl .str_1
    dl .str_2
    dl .str_3
    dl .str_4
    dl .str_5
    dl .str_6
    dl .str_7
.mode_tbl:
    dl mode_vram_test
    dl mode_bram_test
    dl mode_tile_test
    dl mode_vram_size_test
    dl mode_bitmap_test
    dl mode_sprite_test
    dl mode_scroll_test
    dl mode_bitmap_overlay_test
.str_0:
    db "VRAM write/read test",0
.str_1:
    db "$Bxxxxx write/read test",0
.str_2:
    db "Tile test",0
.str_3:
    db "VRAM size test",0
.str_4:
    db "Bitmap mode test",0
.str_5:
    db "Sprite test",0
.str_6:
    db "Scroll test",0
.str_7:
    db "Bitmap overlay test",0
    
    
.titletext:
    db "CYCLONE TEST   MAIN MENU",0
.helptext1:
    db "[Start] Return to menu",0
.helptext2:
    db "[+Pad] Navigate  [C] Select",0
.authtext1:
    db "Coded by Karmic, Jan. 01-08, 2022",0
.authtext2:
    db "karmic128.neocities.org/sgflashback",0
    
    ifd EMU_COMPATIBLE
.emutext:
    db "(EMULATOR COMPATIBILITY MODE ACTIVE)",0
    endif
    




    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; this test indicates that there is NO VRAM encryption/scrambling
    align 1
mode_vram_test:
    
    pushsection
    offset
    align 4
.write_buf:
    ds.b $30
    
.base_address:
    dw 0
    
.write_18:
    db 0
.write_mode:
    db 0
.write_0:
    dw 0
.write_b00012:
    dw 0
.write_b0001a:
    dw 0
.write_b01036:
    dw 0
.write_b01068:
    dw 0

.read_18:
    db 0
.read_mode:
    db 0
.read_0:
    dw 0
.read_b00012:
    dw 0
.read_b0001a:
    dw 0
.read_b01036:
    dw 0
.read_b01068:
    dw 0
    
.cursor_section:
    db 0
.cursor_index:
    db 0
    popsection
    
    bsr full_init_vdp
    
    lea .titletext,a2
    move.l #BGABASE+(SCREENWIDTH*2*1)+32,d0
    bsr output_string_normal
    lea .helptext1,a2
    move.l #BGABASE+(SCREENWIDTH*2*25)+12,d0
    bsr output_string_normal
    lea .helptext2,a2
    move.l #BGABASE+(SCREENWIDTH*2*26)+6,d0
    bsr output_string_normal
    
    
.rerender:
    SetVDPReg $24,1
    
    
    ;show base address
    move.l #BGABASE+(SCREENWIDTH*2*3)+20,d0
    lea .basetext,a2
    move.w .base_address,d2
    bsr output_string_normal
    
    
    
    ;show write header
    move.l #BGABASE+(SCREENWIDTH*2*5)+2,d0
    lea .regstext1,a2
    move.b .write_18,d2
    move.w .write_0,d3
    move.w .write_b00012,d4
    bsr output_string_normal
    move.l #BGABASE+(SCREENWIDTH*2*6)+2,d0
    lea .regstext2,a2
    move.w .write_b0001a,d2
    move.w .write_b01036,d3
    move.w .write_b01068,d4
    bsr output_string_normal
    
    move.l #BGABASE+(SCREENWIDTH*2*5)+24,d0
    moveq #0,d2
    move.b .write_mode,d2
    lea .mode_text_tbl,a2
    lsl.b #2,d2
    movea.l (a2,d2),a2
    bsr output_string_normal
    
    
    ;show write bytes
    lea .write_buf,a3
    move.l #BGABASE+(SCREENWIDTH*2*8)+8,d0
    move.w .base_address,d2
.rowloop1:
    movem.l d0/d2,-(a7)
    lea .rowtext,a2
    bsr output_string_normal
    
    moveq #0,d0
    lea .rowmaintext,a2
    move.b (a3)+,d2
    move.b (a3)+,d3
    move.b (a3)+,d4
    move.b (a3)+,d5
    bsr output_string_normal
    lea .rowmaintext,a2
    move.b (a3)+,d2
    move.b (a3)+,d3
    move.b (a3)+,d4
    move.b (a3)+,d5
    bsr output_string_normal
    
    movem.l (a7)+,d0/d2
    addi.w #8,d2
    addi.l #SCREENWIDTH*2,d0
    cmpi.l #BGABASE+(SCREENWIDTH*2*(8+6))+8,d0
    blo .rowloop1
    
    
    ;write bytes to vram
    bsr init_vdp_transfer
    move.l #$81349800,d0
    or.b .write_18,d0
    move.l d0,(a0)
    move.w .write_0,0
    ifnd EMU_COMPATIBLE
      move.w .write_b00012,$b00012
      move.w .write_b0001a,$b0001a
      move.w .write_b01036,$b01036
      move.w .write_b01068,$b01068
    endif
    
    
    moveq #0,d0
    move.w .base_address,d0
    
    move.b .write_mode,d7
    cmpi.b #2,d7
    beq .write_dma
    
    ConvVDPAddr VRAM_WRITE,d0
    move.l d0,(a0)
    lea .write_buf,a2
    
    tst.b d7
    bne .write_long
.write_word:
    move.w #($30/2)-1,d7
.write_word_loop:
    move.w (a2)+,(a1)
    dbra d7,.write_word_loop
    bra .after_write
    
.write_long:
    move.w #($30/4)-1,d7
.write_long_loop:
    move.l (a2)+,(a1)
    dbra d7,.write_word_loop
    bra .after_write
    
.write_dma:
    SetVDPRegsA $30/2,19, 0,20, (a0)
    SetVDPRegsA ((.write_buf>>1)&$ff),21, ((.write_buf>>9)&$ff),22, (a0)
    SetVDPRegA ((.write_buf>>17)&$7f),23, (a0)
    ConvVDPAddr VRAM_DMA,d0
    move.l d0,(a0)
    bsr wait_vdp_dma
    SetVDPRegA $24,1, (a0)
    bra .after_write
    
.after_write:
    
    
    ;read bytes from vram
    move.w #$9800,d0
    or.b .read_18,d0
    move.w d0,(a0)
    move.w .read_0,0
    ifnd EMU_COMPATIBLE
      move.w .read_b00012,$b00012
      move.w .read_b0001a,$b0001a
      move.w .read_b01036,$b01036
      move.w .read_b01068,$b01068
    endif
    moveq #0,d0
    move.w .base_address,d0
    ConvVDPAddr VRAM_READ,d0
    move.l d0,(a0)
    
    lea -$30(a7),a7 ;allocate stack space
    lea (a7),a2
    
    
    tst.b .read_mode
    bne .read_long
.read_word:
    move.w #($30/2)-1,d7
.read_word_loop:
    move.w (a1),(a2)+
    dbra d7,.read_word_loop
    bra .after_read
    
.read_long:
    move.w #($30/4)-1,d7
.read_long_loop:
    move.l (a1),(a2)+
    dbra d7,.read_long_loop
    bra .after_read
    
.after_read:
    
    move.w #$9800,(a0)
    
    
    ;show read header
    move.l #BGABASE+(SCREENWIDTH*2*15)+2,d0
    lea .regstext1,a2
    move.b .read_18,d2
    move.w .read_0,d3
    move.w .read_b00012,d4
    bsr output_string_normal
    move.l #BGABASE+(SCREENWIDTH*2*16)+2,d0
    lea .regstext2,a2
    move.w .read_b0001a,d2
    move.w .read_b01036,d3
    move.w .read_b01068,d4
    bsr output_string_normal
    
    move.l #BGABASE+(SCREENWIDTH*2*15)+24,d0
    moveq #0,d2
    move.b .read_mode,d2
    lea .mode_text_tbl,a2
    lsl.b #2,d2
    movea.l (a2,d2),a2
    bsr output_string_normal
    
    
    ;display read bytes
    lea (a7),a3
    move.l #BGABASE+(SCREENWIDTH*2*18)+8,d0
    move.w .base_address,d2
.rowloop2:
    movem.l d0/d2,-(a7)
    lea .rowtext,a2
    bsr output_string_normal
    
    moveq #0,d0
    lea .rowmaintext,a2
    move.b (a3)+,d2
    move.b (a3)+,d3
    move.b (a3)+,d4
    move.b (a3)+,d5
    bsr output_string_normal
    lea .rowmaintext,a2
    move.b (a3)+,d2
    move.b (a3)+,d3
    move.b (a3)+,d4
    move.b (a3)+,d5
    bsr output_string_normal
    
    movem.l (a7)+,d0/d2
    addi.w #8,d2
    addi.l #SCREENWIDTH*2,d0
    cmpi.l #BGABASE+(SCREENWIDTH*2*(18+6))+8,d0
    blo .rowloop2
    
    lea $30(a7),a7 ;deallocate
    
    
    
    
    
    move.l #.main,vblank_mode_ptr
    rts
    
    
.main:
    SetVDPReg $64,1
    
    ;un-color old cursor position
    moveq #0,d0
    moveq #0,d1
    move.b .cursor_section,d0
    move.b .cursor_index,d1
    bsr .get_cursor_vram
    move.l d0,d1
    ori.l #VRAM_READ,d0
    ori.l #VRAM_WRITE,d1
    
    bsr init_vdp_transfer
    move.l d0,(a0)
    move.w (a1),d2
    andi.w #$07ff,d2
    move.l d1,(a0)
    move.w d2,(a1)
    
    
    ;handle navigation
    moveq #0,d0
    move.b .cursor_section,d0
    lsl.b #2,d0
    lea .nav_tbl,a0
    movea.l (a0,d0),a0
    move.b .cursor_index,d0
    move.b joy,d1
    jsr (a0)
    
    
    ;color new cursor position
    moveq #0,d0
    moveq #0,d1
    move.b .cursor_section,d0
    move.b .cursor_index,d1
    bsr .get_cursor_vram
    move.l d0,d1
    ori.l #VRAM_READ,d0
    ori.l #VRAM_WRITE,d1
    
    bsr init_vdp_transfer
    move.l d0,(a0)
    move.w (a1),d2
    andi.w #$07ff,d2
    ori.w #$4000,d2
    move.l d1,(a0)
    move.w d2,(a1)
    
    
    ;;;;VBLANK UNSAFE
    cmpi.b #$50,a_hold_timer
    bne .main_do
    moveq #0,d0
    move.w #($30/4)-1,d1
    lea .write_buf,a0
.reset_clr_loop:
    move.l d0,(a0)+
    dbra d1,.reset_clr_loop
    move.w #$1000,.base_address
    move.b d0,.write_18
    move.w d0,.write_0
    move.w d0,.write_b00012
    move.w d0,.write_b0001a
    move.w d0,.write_b01036
    move.w d0,.write_b01068
    move.b d0,.read_18
    move.w d0,.read_0
    move.w d0,.read_b00012
    move.w d0,.read_b0001a
    move.w d0,.read_b01036
    move.w d0,.read_b01068
.main_reset:
    move.l #.rerender,vblank_mode_ptr
.main_end:
    rts
    
.main_do:
    lea .main_tbl,a0
    moveq #0,d0
    move.b .cursor_section,d0
    lsl.b #2,d0
    movea.l (a0,d0),a0
    move.b .cursor_index,d0
    move.b joy,d1
    jmp (a0)
    
    
    
    ;d0=index, d1=joy
.main_tbl:
    dl .main_base
    dl .main_write
    dl .main_bytes
    dl .main_read
    
.main_base:
    cmpi.b #3,d0
    beq .main_end
    move.l d0,d2
    eor.b #3,d2
    move.w .base_address,d0
    bsr do_inc_dec
    beq .main_end
    cmpi.w #$1000,d0 ;lock to $1000-$7fc0 range
    blo .main_end
    cmpi.w #$7fc0,d0
    bhi .main_end
    move.w d0,.base_address
    bra .main_reset
    
    
.main_write:
    lea .write_18,a0
    bra .main_regs
.main_read:
    lea .read_18,a0
    bra .main_regs
    
.main_regs:
    cmpi.b #2,d0
    blo .main_regs_18
    beq .main_regs_mode
.main_regs_words:
    subi.b #3,d0
    move.l d0,d7
    ;get shift count
    not.b d0
    andi.b #3,d0
    move.l d0,d2
    ;get index from a0
    lsr.b #1,d7
    bclr #0,d7
    addi.b #.write_0-.write_18,d7
    ;do change
    move.w (a0,d7),d0
    bsr do_inc_dec
    beq .main_end
    move.w d0,(a0,d7)
    bra .main_reset
    
.main_regs_18:
    bchg #0,d0
    move.l d0,d2
    move.b (a0),d0
    bsr do_inc_dec
    beq .main_end
    move.b d0,(a0)
    bra .main_reset
    
.main_regs_mode:
    move.b 1(a0),d0
    moveq #0,d2
    bsr do_inc_dec
    beq .main_end
    moveq #3,d7 ;max value
    cmpa.l #.write_18,a0
    beq .main_regs_mode_write
    moveq #2,d7
.main_regs_mode_write:
    tst.b d0
    bmi .main_regs_mode_dec
    cmp.b d7,d0
    blo .main_regs_mode_set
    moveq #0,d0
    bra .main_regs_mode_set
.main_regs_mode_dec:
    move.l d7,d0
    subi.b #1,d0
.main_regs_mode_set:
    move.b d0,1(a0)
    bra .main_reset
    
    
    
.main_bytes:
    lea .write_buf,a0
    move.l d0,d4 ;get byte index
    lsr.l #1,d4
    
    andi.b #1,d0
    bchg #0,d0
    move.l d0,d2
    move.b (a0,d4),d0
    bsr do_inc_dec
    beq .main_end
    move.b d0,(a0,d4)
    bra .main_reset
    
    
    
    
    
    ;d0=index, d1=joy
.nav_tbl:
    dl .nav_base
    dl .nav_write
    dl .nav_bytes
    dl .nav_read
    
    
.nav_base:
    btst #BTN_L,d1
    bne .nav_base_l
    btst #BTN_R,d1
    bne .nav_base_r
    btst #BTN_D,d1
    beq .nav_skip
.nav_base_d:
    move.b #6,d0
    bra .nav_down_end
.nav_base_l:
    subi.b #1,d0
    bra .nav_base_set
.nav_base_r:
    addi.b #1,d0
.nav_base_set:
    andi.b #3,d0
    bra .nav_set
    
    
.nav_write:
    moveq #0,d7
    bra .nav_regs
.nav_read:
    moveq #1,d7
    bra .nav_regs
    
    
.nav_regs:
    btst #BTN_L,d1
    bne .nav_regs_l
    btst #BTN_R,d1
    bne .nav_regs_r
    btst #BTN_U,d1
    bne .nav_regs_u
    btst #BTN_D,d1
    beq .nav_skip
.nav_regs_d:
    cmpi.b #$0b,d0
    bhs .nav_regs_d_bottom
.nav_regs_d_top:
    cmpi.b #2,d0
    blo .nav_regs_d_top_set
    cmpi.b #3,d0
    bhs .nav_regs_d_top_3
    moveq #3+$0b,d0
    bra .nav_set
.nav_regs_d_top_3:
    addi.b #1,d0
.nav_regs_d_top_set:
    addi.b #$0b,d0
    bra .nav_set
    
.nav_regs_d_bottom:
    tst.b d7
    bne .nav_skip ;can't go down any further from read
    lea .write_to_bytes_tbl-$0b,a0
    move.b (a0,d0),d0
    bra .nav_down_end
    
    
.nav_regs_u:
    cmpi.b #$0b,d0
    bhs .nav_regs_u_bottom
    tst.b d7
    bne .nav_read_u_top
.nav_write_u_top:
    move.l d0,d1
    moveq #0,d0
    cmpi.b #6,d1
    bls .nav_up_end
    moveq #3,d0
    bra .nav_up_end
.nav_read_u_top:
    lea .read_to_bytes_tbl,a0
    move.b (a0,d0),d0
    bra .nav_up_end
    
.nav_regs_u_bottom:
    cmpi.b #$0f,d0
    bhs .nav_regs_u_bottom_r
    cmpi.b #$0c,d0
    bls .nav_regs_u_bottom_l
    moveq #2,d0
    bra .nav_set
.nav_regs_u_bottom_l:
    subi.b #$0b,d0
    bra .nav_set
.nav_regs_u_bottom_r:
    subi.b #$0f-$03,d0
    bra .nav_set
    
    
.nav_regs_r:
    cmpi.b #23-1,d0
    beq .nav_skip
    addi.b #1,d0
    bra .nav_set
.nav_regs_l:
    tst.b d0
    beq .nav_skip
    subi.b #1,d0
    bra .nav_set
    
    
    
.nav_bytes:
    btst #BTN_L,d1
    bne .nav_bytes_l
    btst #BTN_R,d1
    bne .nav_bytes_r
    btst #BTN_U,d1
    bne .nav_bytes_u
    btst #BTN_D,d1
    beq .nav_skip
    addi.b #$10,d0
    cmpi.b #$60,d0
    blo .nav_set
    andi.b #$0f,d0
    lea .bytes_to_read_tbl,a0
    move.b (a0,d0),d0
.nav_down_end:
    addi.b #1,.cursor_section
    bra .nav_set
    
.nav_bytes_u:
    move.l d0,d7 ;save old index
    subi.b #$10,d0
    bcc .nav_set
    lea .bytes_to_write_tbl,a0
    move.b (a0,d7),d0
.nav_up_end:
    subi.b #1,.cursor_section
    bra .nav_set
    
.nav_bytes_r:
    addi.b #1,d0
    cmpi.b #$60,d0
    blo .nav_set
    moveq #0,d0
    bra .nav_set
.nav_bytes_l:
    subi.b #1,d0
    bcc .nav_set
    moveq #$5f,d0
    bra .nav_set
    
    
    
.nav_set:
    move.b d0,.cursor_index
.nav_skip:
    rts
    
    
    
    
.bytes_to_write_tbl:
    db $0e,$0e,$0e,$0e
    db $0f,$0f,$0f,$0f
    db $11,$12,$12,$12
    db $13,$13,$13,$13
.bytes_to_read_tbl:
    db $02,$02,$02,$02
    db $03,$03,$03,$03
    db $05,$06,$06,$06
    db $07,$07,$07,$07

.read_to_bytes_tbl:
    db $50,$50
    db $50
    db $57,$57,$58,$59
    db $5f,$5f,$5f,$5f
.write_to_bytes_tbl: ;starting at $0b
    db $00,$00,$00,$00
    db $07,$07,$08,$09
    db $0f,$0f,$0f,$0f
    align 1
    
    
    
    ;d0=sec, d1=index
.get_cursor_vram:
    lsl.b #2,d0
    lea .gcv_tbl,a0
    movea.l (a0,d0),a0
    move.l d1,d0
    jmp (a0)
    
.gcv_tbl:
    dl .gcv_base
    dl .gcv_write
    dl .gcv_bytes
    dl .gcv_read
    
.gcv_base:
    add.w d0,d0
    addi.w #BGABASE+(SCREENWIDTH*2*3)+50,d0
    bra .gcv_end
    
    
.gcv_bytes:
    andi.b #$0f,d0 ;nybble-index in d0
    lsr.b #4,d1 ;row in d1
    
    ;nybbleindex += floor(nybbleindex/2)
    move.l d0,d2
    lsr.b #1,d2
    add.b d2,d0
    ;nybbleindex *= 2
    add.b d0,d0
    ;row *= SCREENWIDTH*2
    lsl.w #SCREENWIDTH_SHIFT,d1
    ;screenaddr = ____ + rowindex + nybbleindex
    add.w d1,d0
    addi.w #BGABASE+(SCREENWIDTH*2*8)+24,d0
    bra .gcv_end
    
    
.gcv_write:
    move.l #BGABASE+(SCREENWIDTH*2*5)+18,d7
    bra .gcv_regs
.gcv_read:
    move.l #BGABASE+(SCREENWIDTH*2*15)+18,d7
    
.gcv_regs:
    lea .gcv_regs_tbl,a0
    move.b (a0,d0),d0
    add.b d0,d0
    add.l d7,d0
    bra .gcv_end
    
    
.gcv_end:
    ConvVDPAddr 0,d0
    rts
    
.gcv_regs_tbl:
    db 0,1 ;reg 18
    db 3 ;xfer mode
    db 13,14,15,16 ;0
    db 26,27,28,29 ;$b0012
    ;
    db 24+40,24+41,24+42,24+43 ;$b001a
    db 24+53,24+54,24+55,24+56 ;$b1036
    db 24+66,24+67,24+68,24+69 ;$b1068
    
    
    
.titletext:
    db "VRAM TEST",0
    
.helptext1:
    db "[B] Decrement  [C] Increment",0
.helptext2:
    db "[+Pad] Navigate  [A] Hold to clear",0
    
.basetext:
    db "Base address: $",FMT_WORD,0

    ;there should be an extra 0 after the $b's but there's not enough space
.regstext1:
    db "reg$18:$",FMT_BYTE,"       $0:$",FMT_WORD," $b0012:$",FMT_WORD,0
.regstext2:
    db "$b001a:$",FMT_WORD," $b1036:$",FMT_WORD," $b1068:$",FMT_WORD,0
    
    align 1
.mode_text_tbl:
    dl .modetext1
    dl .modetext2
    dl .modetext3
.modetext1:
    db "Word",0
.modetext2:
    db "Long",0
.modetext3:
    db "DMA",0
    
.rowtext:
    db "$",FMT_WORD,":  ",0
.rowmaintext:
    db FMT_BYTE," "
    db FMT_BYTE," "
    db FMT_BYTE," "
    db FMT_BYTE," "
    db 0
    
    
    
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;
    align 1
mode_bram_test:
    bsr full_init_vdp
    
    move.l #BGABASE+(SCREENWIDTH*2*2)+28,d0
    lea .titletext,a2
    bsr output_string_normal
    
    
    
    
    ;do test
    ifnd EMU_COMPATIBLE
      lea $b00000,a3
      lea $c00000,a4
    else
      lea $ff0000,a3
      lea $ff4000,a4
    endif
    moveq #0,d7 ;found count
    
    move.l #BGABASE+(SCREENWIDTH*2*5)+8,d6
    
.test_loop:
    move.w #$5555,d0
    move.w d0,(a3)
    cmp.w (a3),d0
    bne .next
    add.w d0,d0
    move.w d0,(a3)
    cmp.w (a3),d0
    bne .next
    
    move.l d7,d0 ;column
    andi.b #3,d0
    lsl.l #4,d0
    
    move.l d7,d1
    lsr.l #2,d1
    lsl.l #SCREENWIDTH_SHIFT,d1
    
    add.l d1,d0
    add.l d6,d0
    move.l a3,d2
    lea .addrtext,a2
    bsr output_string_normal
    
    addi.b #1,d7
    cmpi.b #88,d7
    bhs .test_end
.next:
    adda.l #2,a3
    cmpa.l a4,a3
    blo .test_loop
.test_end:
    
    
    tst.b d7
    bne .end
    
    move.l #BGABASE+(SCREENWIDTH*2*5)+8,d0
    lea .notext,a2
    bsr output_string_normal
    
    
.end:
    move.l #mode_screenon, vblank_mode_ptr
    rts
    
    
    
.titletext:
    db "$Bxxxxx TEST",0
    
.addrtext:
    db "$",FMT_24,0
    
.notext:
    db "No write/readable addresses found",0
    
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; this test indicates:
    ;;  colors are 4 bytes: green, red, 0, then blue
    ;;    are color depths full 8-bit?
    ;;  palettes are 64 colors ($100 bytes) long
    ;;  tiles to fill in a pixel:
    ;;    0: 00 3f 00 00 00 00
    ;;    1: 0f c0 00 00 00 00
    ;;    2: f0 00 00 03 00 00
    ;;    3: 00 00 00 fc 00 00
    ;;    4: 00 00 3f 00 00 00
    ;;    5: 00 00 c0 00 00 0f
    ;;    6: 00 00 00 00 03 f0
    ;;    7: 00 00 00 00 fc 00
    ;;  the arrangement makes more sense if you shuffle the bytes:
    ;;      to: 4 5 2 3 0 1
    ;;    when you do this, the tile row data is arranged like so;
    ;;      77777766 66665555 55444444 33333322 22221111 11000000
    ;;    where "7" is the color of the rightmost pixel and "0" the color of the left
    ;;  tiles are $30 bytes long
    ;;  tilemap flip/priority bits still work as expected
    ;;  $bxxxxx locations do not impact the screen display
    ;;  higher VRAM/CRAM addresses are mirrored when cyclone mode is off
    ;;  since there are more colors, the background color register #7 is extended
    align 1
mode_tile_test:
    
    pushsection
    offset
    align 4
    
.palette:
    ds.l 64
.tile_data:
    ds.b $30
    
._18:
    db 0
.cursor_section:
    db 0
.tile:
    dw 0
._0:
    dw 0
.b00012:
    dw 0
.b0001a:
    dw 0
.b01036:
    dw 0
.b01068:
    dw 0
    
.palette_base:
    dw 0
.tile_base:
    dw 0
    
.cursor_index:
    dw 0
    
    popsection
    
    
    bsr full_init_vdp
    
    ;title text
    move.l #BGABASE+(SCREENWIDTH*2*0)+30,d0
    lea .titletext,a2
    bsr output_string_normal
    
    ;clear BG B
    ifnd EMU_COMPATIBLE
      SetVDPAddrA BGBBASE,VRAM_WRITE,(a0)
      move.w #(SCREENWIDTH*SCREENHEIGHT/2)-1,d7
      move.w #($1020/30),d0
.clear_bgb_loop:
      move.w d0,(a1)
      dbra d7,.clear_bgb_loop
    endif
    
    
.rerender:
    bsr init_vdp_transfer
    SetVDPRegA $24,1, (a0)
    
    
    
    ;register display
    move.l #BGABASE+(SCREENWIDTH*2*1),d0
    lea .regstext1,a2
    move.b ._18,d2
    move.w .tile,d3
    move.w ._0,d4
    move.w .b00012,d5
    bsr output_string_normal
    
    move.l #BGABASE+(SCREENWIDTH*2*2)+2,d0
    move.w .b0001a,d2
    move.w .b01036,d3
    move.w .b01068,d4
    bsr output_string_normal
    
    
    ;tile data
    lea .tile_data,a3
    move.w .tile_base,d2
    move.l #BGABASE+(SCREENWIDTH*2*3)+16,d0
.tile_data_row_loop:
    movem.l d0/d2,-(a7)
    lea .tilerowtext,a2
    bsr output_string_normal
    
    moveq #0,d0
    move.b (a3)+,d2
    move.b (a3)+,d3
    move.b (a3)+,d4
    move.b (a3)+,d5
    move.b (a3)+,d6
    move.b (a3)+,d7
    bsr output_string_normal
    
    movem.l (a7)+,d0/d2
    addi.w #6,d2
    addi.l #SCREENWIDTH*2,d0
    cmpi.l #BGABASE+(SCREENWIDTH*2*(3+8))+10,d0
    blo .tile_data_row_loop
    
    
    ;tile display row
    SetVDPAddrA BGABASE+(SCREENWIDTH*2*11),VRAM_WRITE,(a0)
    moveq #SCREENWIDTH-1,d7
    move.w .tile,d0
.tile_display_loop:
    move.w d0,(a1)
    dbra d7,.tile_display_loop
    
    
    ;palette data
    lea .palette,a3
    move.w .palette_base,d2
    move.l #BGABASE+(SCREENWIDTH*2*12),d0
.palette_row_loop:
    move.l d0,-(a7)
    
    lea .paletterowtext,a2
    move.l (a3)+,d3
    move.l (a3)+,d4
    move.l (a3)+,d5
    move.l (a3)+,d6
    bsr output_string_normal
    
    move.l (a7)+,d0
    addi.w #$10,d2
    addi.l #SCREENWIDTH*2,d0
    cmpi.l #BGABASE+(SCREENWIDTH*2*(12+16))+10,d0
    blo .palette_row_loop
    
    
    
    ;write cyclone registers
    SetVDPRegA $80,$18, (a0)
    move.w ._0,0
    ifnd EMU_COMPATIBLE
      move.w .b00012,$b00012
      move.w .b0001a,$b0001a
      move.w .b01036,$b01036
      move.w .b01068,$b01068
    endif
    
    
    
    ;dma tile to vram
    SetVDPRegsA $34,1, ($30/2),19, (a0)
    SetVDPRegsA 0,20, ((.tile_data>>1)&$ff),21, (a0)
    SetVDPRegsA ((.tile_data>>9)&$ff),22, ((.tile_data>>17)&$7f),23, (a0)
    moveq #0,d0
    move.w .tile_base,d0
    ConvVDPAddr VRAM_DMA,d0
    move.l d0,(a0)
    bsr wait_vdp_dma
    SetVDPRegA $24,1, (a0)
    
    
    
    ;dma palette
    ifnd EMU_COMPATIBLE
      SetVDPRegsA $34,1, ($100/2),19, (a0)
      SetVDPRegsA 0,20, ((.palette>>1)&$ff),21, (a0)
      SetVDPRegsA ((.palette>>9)&$ff),22, ((.palette>>17)&$7f),23, (a0)
      moveq #0,d0
      move.w .palette_base,d0
      ConvVDPAddr CRAM_DMA,d0
      move.l d0,(a0)
      bsr wait_vdp_dma
      SetVDPRegA $24,1, (a0)
    endif
    
    SetVDPRegA 0,$18, (a0)
    
    
    
    move.l #.main,vblank_mode_ptr
    rts
    
    
.main:
    bsr init_vdp_transfer
    
    SetVDPRegA $64,1, (a0)
    
    
    ;un-color previous cursor pos
    moveq #0,d0
    moveq #0,d1
    move.b .cursor_section,d0
    move.w .cursor_index,d1
    bsr .get_cursor_vram
    move.l d0,d1
    ori.l #VRAM_READ,d0
    ori.l #VRAM_WRITE,d1
    
    move.l d0,(a0)
    move.w (a1),d2
    andi.w #$07ff,d2
    move.l d1,(a0)
    move.w d2,(a1)
    
    
    ;do navigation
    moveq #0,d0
    moveq #0,d2
    move.w .cursor_index,d0
    move.b joy,d1
    move.b .cursor_section,d2
    lea .nav_tbl,a0
    lsl.b #2,d2
    movea.l (a0,d2),a0
    jsr (a0)
    
    
    ;color new cursor pos
    bsr init_vdp_transfer
    moveq #0,d0
    moveq #0,d1
    move.b .cursor_section,d0
    move.w .cursor_index,d1
    bsr .get_cursor_vram
    move.l d0,d1
    ori.l #VRAM_READ,d0
    ori.l #VRAM_WRITE,d1
    
    move.l d0,(a0)
    move.w (a1),d2
    andi.w #$07ff,d2
    ori.w #$4000,d2
    move.l d1,(a0)
    move.w d2,(a1)
    
    
    
    ;;;;; VBLANK UNSAFE
    
    ;switch to cyclone mode mid-frame
    bsr init_vdp_transfer
    
.raster_loop_0:
    btst.b #3,1(a0) ;wait until out of vblank
    bne .raster_loop_0
    
    lea HVCOUNTER-VDPCTRL(a0),a2
    nop
.raster_loop_1:
    cmpi.b #$57,(a2)
    bne .raster_loop_1
    
    move.w #$9800,d0
    or.b ._18,d0
    move.w d0,(a0)
    ifnd EMU_COMPATIBLE
      SetVDPRegA $40,7, (a0)
    else
      SetVDPRegA $20,7, (a0)
    endif
    
    
.raster_loop_2:
    cmpi.b #$5f,(a2)
    bne .raster_loop_2
    
    SetVDPRegA 0,$18, (a0)
    SetVDPRegA 0,7, (a0)
    
    
    
    ;hold a to reset
    cmpi.b #$50,a_hold_timer
    bne .main_no_reset
    moveq #0,d0
    move.b d0,._18
    move.w #($1050/$30) | $2000,.tile
    move.w d0,._0
    move.w d0,.b00012
    move.w d0,.b0001a
    move.w d0,.b01036
    move.w d0,.b01068
    move.w #$1050, .tile_base
    move.w #$0100, .palette_base
    
    lea .tile_data,a0
    move.w #($30/4)-1,d7
.tile_clear_loop:
    move.l d0,(a0)+
    dbra d7,.tile_clear_loop
    
    lea .palette,a0
.palclr:
    move.l d0,(a0)+
    addi.l #$04040404,d0
    bcc .palclr
    
.main_reset:
    move.l #.rerender,vblank_mode_ptr
.main_end:
    rts
    
    
.main_no_reset:
    ;do main stuff
    lea .main_tbl,a0
    moveq #0,d0
    move.b .cursor_section,d0
    lsl.b #2,d0
    movea.l (a0,d0),a0
    move.w .cursor_index,d0
    move.b joy,d1
    jmp (a0)
    
    
    
.main_tbl:
    dl .main_regs
    dl .main_tile
    dl .main_palette
    
    
.main_regs:
    lea ._18,a0
    cmpi.w #2,d0
    blo .main_regs_18
    lea .tile-._18(a0),a0
    subi.w #2,d0
    move.l d0,d2
    not.w d2
    andi.w #3,d2
    move.l d0,d7
    lsr.w #1,d7
    bclr #0,d7
    move.w (a0,d7),d0
    bsr do_inc_dec
    beq .main_end
    move.w d0,(a0,d7)
    bra .main_reset
    
.main_regs_18:
    move.l d0,d2
    bchg #0,d2
    move.b (a0),d0
    bsr do_inc_dec
    beq .main_end
    move.b d0,(a0)
    bra .main_reset
    
    
    
.main_tile:
    ;get row index
    move.l d0,d7
    andi.w #$0f,d7
    cmpi.w #4,d7
    blo .main_tile_base
    lea .tile_data,a0
    ;get nibble
    moveq #0,d2
    move.b d7,d2
    not.b d2
    andi.b #1,d2
    ;get byte index
    subi.w #4,d7
    lsr.w #1,d7
    ;get byte row index ((index>>4) * 6)
    lsr.w #4,d0
    move.l d0,d4
    lsl.l #1,d0
    lsl.l #2,d4
    add.l d0,d4
    add.l d7,d4
    ;now actually modify
    move.b (a0,d4),d0
    bsr do_inc_dec
    beq .main_end
    move.b d0,(a0,d4)
    bra .main_reset
    
.main_tile_base:
    move.l d7,d2
    not.b d2
    andi.b #$03,d2 ;don't change lower nybble
    beq .main_end
    move.w .tile_base,d0
    bsr do_inc_dec
    beq .main_end
    cmpi.w #$1050,d0
    blo .main_end
    cmpi.w #$7fc0,d0
    bhi .main_end
    move.w d0,.tile_base
    bra .main_reset
    
    
.main_palette:
    divu.w #(8*4)+4,d0 ;slow but i don't care
    cmpi.l #$00040000,d0
    blo .main_palette_base
    lea .palette,a0
    moveq #0,d7
    move.w d0,d7
    ;get byte row index
    lsl.l #4,d7
    ;get nibble
    clr.w d0
    swap d0
      moveq #0,d2 ;get nibble
      btst #0,d0
      bne .main_palette_nib
      moveq #1,d2
.main_palette_nib:
    ;get byte column index
    subi.w #4,d0
    lsr.w #1,d0
      
    add.l d0,d7
    ;do actual write
    move.b (a0,d7),d0
    bsr do_inc_dec
    beq .main_end
    move.b d0,(a0,d7)
    bra .main_reset
    
    
.main_palette_base:
    clr.w d0
    swap d0
    move.l d0,d2
    not.b d2
    andi.b #3,d2
    beq .main_end
    cmpi.b #3,d2
    beq .main_end
    move.w .palette_base,d0
    bsr do_inc_dec
    beq .main_end
    cmpi.w #$0100,d0
    blt .main_end
    cmpi.w #$0700,d0
    bgt .main_end
    move.w d0,.palette_base
    bra .main_reset
    
    
    
.nav_tbl:
    dl .nav_regs
    dl .nav_tile
    dl .nav_palette
    
    
.nav_regs:
    btst #BTN_U,d1
    bne .nav_regs_u
    btst #BTN_D,d1
    bne .nav_regs_d
    btst #BTN_L,d1
    bne .nav_regs_l
    btst #BTN_R,d1
    beq .nav_skip
.nav_regs_r:
    cmpi.w #26-1,d0
    bhs .nav_skip
    addi.w #1,d0
    bra .nav_set
.nav_regs_l:
    tst.w d0
    beq .nav_skip
    subi.w #1,d0
    bra .nav_set
    
.nav_regs_u:
    cmpi.w #$0e,d0 ;can't go any higher?
    blo .nav_skip
    lea .regs_lower_to_upper_tbl-$0e,a0
    move.b (a0,d0),d0
    bra .nav_set
    
.nav_regs_d:
    cmpi.w #$0e,d0
    bhs .nav_regs_d_down
    lea .regs_upper_to_lower_tbl,a0
    move.b (a0,d0),d0
    bra .nav_set
.nav_regs_d_down:
    subi.w #$0e,d0
    cmpi.w #$04,d0
    blo .nav_down
    cmpi.w #$08,d0
    blo .nav_regs_d_down_mid
    moveq #$0f,d0
    bra .nav_down
.nav_regs_d_down_mid:
    cmpi.w #$06,d0
    bhs .nav_regs_d_down_mid_2
    moveq #9,d0
    bra .nav_down
.nav_regs_d_down_mid_2:
    subi.w #$06,d0
    addi.w #$0a,d0
.nav_down:
    addi.b #1,.cursor_section
    bra .nav_set
    
    
.nav_tile:
    btst #BTN_U,d1
    bne .nav_tile_u
    btst #BTN_D,d1
    bne .nav_tile_d
    btst #BTN_L,d1
    bne .nav_tile_l
    btst #BTN_R,d1
    beq .nav_skip
.nav_tile_r:
    cmpi.w #$80-1,d0
    bhs .nav_skip
    addi.w #1,d0
    bra .nav_set
.nav_tile_l:
    tst.w d0
    beq .nav_skip
    subi.w #1,d0
    bra .nav_set
    
.nav_tile_u:
    cmpi.w #$10,d0
    blo .nav_tile_u_up
    subi.w #$10,d0
    bra .nav_set
.nav_tile_u_up:
    lea .tile_to_regs_tbl,a0
    move.b (a0,d0),d0
.nav_tile_u_up_set:
    addi.w #$0e,d0
.nav_up:
    subi.b #1,.cursor_section
    bra .nav_set
    
.nav_tile_d:
    cmpi.w #$70,d0
    bhs .nav_tile_d_down
    addi.w #$10,d0
    bra .nav_set
.nav_tile_d_down:
    lea .tile_to_palette_tbl-$70,a0
    move.b (a0,d0),d0
    bra .nav_down
    
    
.nav_palette:
    btst #BTN_U,d1
    bne .nav_palette_u
    btst #BTN_D,d1
    bne .nav_palette_d
    btst #BTN_L,d1
    bne .nav_palette_l
    btst #BTN_R,d1
    beq .nav_skip
.nav_palette_r:
    cmpi.w #((4*4)+2)*2*16-1,d0
    bhs .nav_skip
    addi.w #1,d0
    bra .nav_set
.nav_palette_l:
    tst.w d0
    beq .nav_skip
    subi.w #1,d0
    bra .nav_set
    
.nav_palette_u:
    cmpi.w #((4*4)+2)*2,d0
    blo .nav_palette_u_up
    subi.w #((4*4)+2)*2,d0
    bra .nav_set
.nav_palette_u_up:
    cmpi.w #$08,d0
    blo .nav_palette_u_up_l
    cmpi.w #$1b,d0
    bhi .nav_palette_u_up_r
    lea .palette_to_tile_tbl-$08,a0
    move.b (a0,d0),d0
    bra .nav_up
    
.nav_palette_u_up_l:
    moveq #$70,d0
    bra .nav_up
.nav_palette_u_up_r:
    moveq #$7f,d0
    bra .nav_up
    
.nav_palette_d:
    cmpi.w #((4*4)+2)*2*15,d0
    bhs .nav_skip
    addi.w #((4*4)+2)*2,d0
    bra .nav_set
    
    
.nav_set:
    move.w d0,.cursor_index
.nav_skip:
    rts
    
    
.regs_upper_to_lower_tbl:
    db $0e,$0e
    db $11,$11,$11,$11
    db $13,$14,$15,$15
    db $17,$18,$19,$19
.regs_lower_to_upper_tbl:
    db $01,$01,$02,$02
    db $06,$06,$07,$08
    db $0a,$0a,$0b,$0c
    
.tile_to_regs_tbl:
    db 0,1,2,3
    db 3,3
    db 4,4
    db 4,4
    db 6,7
    db 7,7
    db 8,8
    
.tile_to_palette_tbl:
    db 8,9,10,11
    db 13,14
    db 16,17
    db 19,19
    db 21,22
    db 24,25
    db 27,27
    
.palette_to_tile_tbl:
    db $70,$71,$72,$73
    db $74,$74,$75,$75,$76,$77,$77,$78
    db $7a,$7a,$7b,$7b,$7c,$7d,$7d,$7e
    
    
    
.get_cursor_vram:
    lsl.b #2,d0
    lea .gcv_tbl,a2
    movea.l (a2,d0),a2
    move.l d1,d0
    jmp (a2)
    
.gcv_tbl:
    dl .gcv_regs
    dl .gcv_tile
    dl .gcv_palette
    
    
.gcv_regs:
    move.l #BGABASE+(SCREENWIDTH*2*1),d7
    lea .gcv_regs_tbl,a2
    move.b (a2,d0),d0
    add.b d0,d0
    add.l d7,d0
    bra .gcv_end
    

.gcv_tile:
    move.l #BGABASE+(SCREENWIDTH*2*3)+(9*2),d7
    ;get row index
    move.l d0,d6
    lsr.l #4,d6
    lsl.l #SCREENWIDTH_SHIFT,d6
    add.l d6,d7
    ;get column index
    andi.b #$0f,d0
    cmpi.b #4,d0
    blo .gcv_tile_set
    subi.b #4,d0
    move.l d0,d6
    lsr.l #1,d6
    add.b d6,d0
    addi.l #6,d0
.gcv_tile_set:
    add.l d0,d0
    add.l d7,d0
    bra .gcv_end
    
    
.gcv_palette:
    move.l #BGABASE+(SCREENWIDTH*2*12),d7
    divu.w #(8*4)+4,d0 ;slow but i don't care
    ;now row is in d0 lower and column in d0 upper
    ;get row index
    moveq #0,d6
    move.w d0,d6
    lsl.l #SCREENWIDTH_SHIFT,d6
    add.l d6,d7
    ;get column index
    clr.w d0
    swap d0
    cmpi.w #4,d0
    blo .gcv_tile_set
    subi.w #4,d0
    move.w d0,d6
    lsr.l #3,d6
    add.w d6,d0
    addi.l #5,d0
    bra .gcv_tile_set
    
    
    
.gcv_end:
    ConvVDPAddr 0,d0
    rts
    
    
.gcv_regs_tbl:
    db $05,$06
    db $0e,$0f,$10,$11
    db $17,$18,$19,$1a
    db $24,$25,$26,$27
    ;
    db $49,$4a,$4b,$4c
    db $56,$57,$58,$59
    db $63,$64,$65,$66
    
    
    
.titletext:
    db "TILE TEST",0
    
.regstext1:
    db "r18:$",FMT_BYTE," Tile:$",FMT_WORD," $0:$",FMT_WORD," $b0012:$",FMT_WORD,0
.regstext2:
    db "$b001a:$",FMT_WORD," $b1036:$",FMT_WORD," $b1068:$",FMT_WORD,0

.tilerowtext:
    db "$",FMT_WORD,": ",0
.tilerowmaintext:
    db FMT_BYTE," ",FMT_BYTE," ",FMT_BYTE," ",FMT_BYTE," ",FMT_BYTE," ",FMT_BYTE,0

.paletterowtext:
    db FMT_WORD,":",FMT_LONG," ",FMT_LONG," ",FMT_LONG," ",FMT_LONG," ",0
    
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  this test indicates:
    ;;    extra ram is only enabled when bit 7 of register $18 is set
    ;;    when reg$18 = $00:
    ;;       VRAM size: $010000 (mirrored)
    ;;       CRAM size: $000080
    ;;      VSRAM size: $000050
    ;;    when reg$18 = $80:
    ;;       VRAM size: $01ff7c
    ;;       CRAM size: $000800 (mirrored)
    ;;      VSRAM size: $000050
    align 1
mode_vram_size_test:
    bsr init_vdp_regs
    
    lea -18*2(a7),a7
    
    ;perform tests
    lea (a7),a2
    bsr .do_test
    
    SetVDPRegA $80,$18, (a0)
    lea 18*1(a7),a2
    bsr .do_test
    
    
    
    ;output results
    bsr full_init_vdp
    
    move.l #BGABASE+(SCREENWIDTH*2*1)+26,d0
    lea .titletext,a2
    bsr output_string_normal
    
    
    move.l #BGABASE+(SCREENWIDTH*2*6),d7
    moveq #0,d6
.test_print_loop: ;this loop cannot ever push anything to stack
    ;header
    moveq #20,d0
    add.l d7,d0
    lea ._18_tbl,a2
    move.b (a2,d6),d2
    lea .headtext,a2
    bsr output_string_normal
    
    
    ;test result rows
    moveq #0,d5
.test_one_loop:
    ;get address
    move.l d5,d0
    lsl.l #SCREENWIDTH_SHIFT-2,d0
    addi.l #(SCREENWIDTH*2*2)+10,d0
    add.l d7,d0
    ;ram type
    lea .type_tbl,a2
    movea.l (a2,d5),a2
    bsr output_string_normal
    ;ram size
    move.l (a7)+,d2
    moveq #0,d0
    lea .resulttext,a2
    bsr output_string_normal
    ;mirror
    move.w (a7)+,d3
    beq .no_print_mir
    moveq #0,d0
    lea .mirtext,a2
    bsr output_string_normal
.no_print_mir:
    addi.l #4,d5
    cmpi.l #4*3,d5
    blo .test_one_loop
    
    
    addi.l #SCREENWIDTH*2*9,d7
    addi.l #1,d6
    cmpi.l #2,d6
    blo .test_print_loop
    
    
    
    ;lea 18*3(a7),a7
    move.l #mode_screenon,vblank_mode_ptr
    rts
    
    
    ;A2 = result storage
.do_test:
    bsr init_vdp_transfer
    
    SetVDPAddrA 0,VRAM_WRITE, d0
    SetVDPAddrA 0,VRAM_READ, d1
    bsr .do_one_test
    move.l d0,0(a2)
    move.w d1,4(a2)
    
    SetVDPAddrA 0,CRAM_WRITE, d0
    SetVDPAddrA 0,CRAM_READ, d1
    bsr .do_one_test
    move.l d0,6(a2)
    move.w d1,10(a2)
    
    SetVDPAddrA 0,VSRAM_WRITE, d0
    SetVDPAddrA 0,VSRAM_READ, d1
    bsr .do_one_test
    move.l d0,12(a2)
    move.w d1,16(a2)
    
    rts
    
    ; in: D0 = write ptr, D1 = read ptr
    ;out: D0 = size, D1 = mirror flag
.do_one_test:
    ;;;NOTE- the emulator crashes if you try writing to $1ff7c-$1ffff or $3ff7c-$3ffff
    ;;;it also crashed the whole system in between
    
    ;write $00000-$1ff7b
    move.l #$55555555,d2
    move.l d0,(a0)
    move.w #($1ff7c/4)-1,d7
.test_write_loop_1:
    move.l d2,(a1)
    dbra d7,.test_write_loop_1
    
    ;write something else to $0000 to detect mirroring
    move.l d2,d3
    add.l d3,d3
    move.l d0,(a0)
    move.l d3,(a1)
    
    
    
    ;read starting at $0004
    bset #16+2,d1
    move.l d1,(a0)
    
    move.w #($1ff7c/4)-1,d7
    moveq #4,d0
.test_read_loop:
    move.l (a1),d6
    cmp.l d3,d6 ;mirrored?
    beq .test_mir
    cmp.l d2,d6 ;end of ram?
    bne .test_end
    addi.l #4,d0
    dbra d7,.test_read_loop
    
.test_end:
    moveq #0,d1
    rts
    
.test_mir:
    moveq #1,d1
    rts
    
    
    
    
.titletext:
    db "VRAM SIZE TEST",0
    
.headtext:
    db "When reg$18 = $",FMT_BYTE,":",0
    
    align 1
.type_tbl:
    dl .type_1
    dl .type_2
    dl .type_3
    
.type_1:
    db " VRAM",0
.type_2:
    db " CRAM",0
.type_3:
    db "VSRAM",0
    
._18_tbl:
    db 0,$80
    
.resulttext:
    db " size: $",FMT_24,0
.mirtext:
    db " (mirrored)",0
    
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;
mode_bitmap_test:

    pushsection
    offset
    align 4
.fill_value:
    dl 0
.cursor_index:
    db 0
.pixel_color:
    db 0
    popsection
    
    
    bsr full_init_vdp
    
    bsr init_vdp_transfer
    SetVDPRegsA ($2000>>10),2, ($4000>>13),4, (a0)
    SetVDPRegsA ($4000>>9),5, ($4000>>10),$0d, (a0)
    
    
    lea .titletext,a2
    move.l #$2000+(SCREENWIDTH*2*1)+24,d0
    bsr output_string_normal
    
    
    ifnd EMU_COMPATIBLE
      SetVDPRegA $80,$18, (a0)
      SetVDPAddrA $4e00+(320*5*8),VRAM_WRITE, (a0)
      move.l #(32<<4) | (32<<10),d0 ;black
      move.w #(320*(224-(5*8)))/4 - 1, d7
.bmp_clear_loop:
      move.l d0,(a1)
      dbra d7,.bmp_clear_loop
    endif
    
    
    
.rerender:
    SetVDPReg $24,1

    lea .maintext,a2
    move.l #$2000+(SCREENWIDTH*2*3)+2,d0
    move.l .fill_value,d2
    move.b .pixel_color,d3
    bsr output_string_normal
    
    
    
    
    ifnd EMU_COMPATIBLE
      SetVDPRegA $80,$18, (a0)
      SetVDPAddrA $4e00+(320*5*8),VRAM_WRITE, (a0)
      move.l .fill_value,d0
      move.w #(320*8)/4 - 1, d7
.bmp_fill_loop:
      move.l d0,(a1)
      dbra d7,.bmp_fill_loop
      
      
      ;draw color-test bitmap
      
      ;get upper-left first pixel value
      moveq #0,d0
      move.b .pixel_color,d0
      move.l d0,d1
      lsl.l #5,d1
      or.l d1,d0
      lsl.l #5,d1
      or.l d1,d0
      lsl.l #5,d1
      or.l d1,d0
      swap d0
      
      move.l #$4e00+(320*9*8)+((320-128)), d7 ;vram pointer
      moveq #64-1,d6 ;double-pixel rows left
.color_bmp_row_loop:
      move.l d7,d1
      ConvVDPAddr VRAM_WRITE,d1
      move.l d1,(a0)
      
      moveq #64-1,d5 ;double-pixel columns left
.color_bmp_col_loop:
      move.l d0,(a1)
      addi.w #$10,d0
      dbra d5,.color_bmp_col_loop
      addi.l #320*2,d7
      dbra d6,.color_bmp_row_loop
      
      
    endif
    
    
    
    
    
    move.l #.main,vblank_mode_ptr
    rts
    
    
.main:
    bsr init_vdp_transfer
    ifnd EMU_COMPATIBLE
      SetVDPRegsA $64,1, $00,$18, (a0)
    else
      SetVDPRegsA $64,1, $00,7, (a0)
    endif
    
    
    
    ;un-color old cursor pos
    moveq #0,d1
    move.b .cursor_index,d1
    move.l #$2000+(SCREENWIDTH*2*3)+2+(13*2),d0
    cmpi.b #8,d1
    blo .uncolor_not_color
    addi.b #15,d1
.uncolor_not_color
    add.l d1,d1
    add.l d1,d0
    ConvVDPAddr 0,d0
    move.l d0,d1
    ori.l #VRAM_READ,d0
    ori.l #VRAM_WRITE,d1
    
    move.l d0,(a0)
    move.w (a1),d2
    andi.w #$07ff,d2
    move.l d1,(a0)
    move.w d2,(a1)
    
    
    ;do navigation
    moveq #0,d0
    move.b .cursor_index,d0
    move.b joy,d1
    
    btst #BTN_L,d1
    bne .nav_l
    btst #BTN_R,d1
    beq .nav_skip
    addi.b #1,d0
    cmpi.b #9,d0
    bls .nav_set
    moveq #0,d0
    bra .nav_set
.nav_l:
    subi.b #1,d0
    bcc .nav_set
    moveq #9,d0
.nav_set:
    move.b d0,.cursor_index
.nav_skip:
    
    
    ;color new cursor pos
    moveq #0,d1
    move.b d0,d1
    move.l #$2000+(SCREENWIDTH*2*3)+2+(13*2),d0
    cmpi.b #8,d1
    blo .color_not_color
    addi.b #15,d1
.color_not_color
    add.l d1,d1
    add.l d1,d0
    ConvVDPAddr 0,d0
    move.l d0,d1
    ori.l #VRAM_READ,d0
    ori.l #VRAM_WRITE,d1
    
    move.l d0,(a0)
    move.w (a1),d2
    andi.w #$07ff,d2
    ori.w #$4000,d2
    move.l d1,(a0)
    move.w d2,(a1)
    
    
    
    
    ;do main stuff
    move.b joy,d1
    
    moveq #0,d2
    move.b .cursor_index,d2
    cmpi.b #8,d2
    bhs .main_pixel
    eori.b #7,d2
    move.l .fill_value,d0
    bsr do_inc_dec
    beq .no_reset
    move.l d0,.fill_value
    bra .do_reset
    
    
.main_pixel:
    not.b d2
    andi.b #1,d2
    move.b .pixel_color,d0
    bsr do_inc_dec
    beq .no_reset
    andi.b #$1f,d0
    move.b d0,.pixel_color

.do_reset:
    move.l #.rerender,vblank_mode_ptr
    bra .no_reset
.no_reset:
    
    
    
    ;turn bitmap mode on mid-frame
    bsr init_vdp_transfer
    lea HVCOUNTER-VDPCTRL(a0),a2
    nop
.raster_loop:
    cmpi.b #$27,(a2)
    bne .raster_loop
    
    ifnd EMU_COMPATIBLE
      SetVDPRegA $c0,$18, (a0)
    else
      SetVDPRegA $12,7, (a0)
    endif
    
    rts
    
    
.titletext:
    db "BITMAP MODE TEST",0
    
.maintext:
    db "Fill value: $",FMT_LONG,"  Pixel color:$",FMT_BYTE,0
    
    
    
    
    
    
    ;; from now on the modes start getting not even a little emu-compatible lol
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;
mode_sprite_test:
    
    pushsection
    offset
    align 4
.sine_index:
    dw 0
.frame_count:
    db 0
    popsection
    
    bsr init_vdp_regs
    bsr clear_vram
    
    bsr init_cram_cyclone
    bsr upload_font_cyclone
    
    
    bsr init_vdp_transfer
    SetVDPRegsA $80,$18, ($1800>>9),5, (a0) ;SAT at $1800
    SetVDPRegsA 8,2, 2,4, (a0) ;BGA at $2000, BGB at $4000
    
    
    move.l #.main,vblank_mode_ptr
    rts
    
    
    
.main:
    bsr init_vdp_transfer
    
    SetVDPRegA $64,1, (a0)
    
    SetVDPAddrA $1800,VRAM_WRITE, (a0)
    
    lea sine_tbl,a2
    lea .spritetext,a3
    
    moveq #.SPRITE_COUNT-1,d7 ;sprites left
    moveq #1,d6 ;sprite size/link
    moveq #0,d5 ;sine index
    move.w .sine_index,d5
    move.w #128+(320-(.SPRITE_COUNT*8))/2,d4 ;x-coord
.sprite_loop:
    ;y-coord
    move.w (a2,d5),d0
    asr.w #8,d0
    asr.w #1,d0
    addi.w #$48 + 128,d0
    move.w d0,(a1)
    addi.w #$0024,d5
    andi.w #$03fe,d5
    ;sprite link
    tst.b d7
    bne .no_reset_link
    moveq #0,d6
.no_reset_link:
    move.w d6,(a1)
    addi.w #1,d6
    ;tile id
    moveq #0,d0 ;palette
    move.b .frame_count,d0
    andi.b #$60,d0
    lsl.l #8,d0
    move.b (a3)+,d0 ;actual tile
    move.w d0,(a1)
    ;x-coord
    move.w d4,(a1)
    addi.w #8,d4
    
    dbra d7,.sprite_loop
    
    
    
    ;change global palette selector
    move.w #$9880,d0
    btst.b #7,.frame_count
    bne .no_pal
    bset #3,d0
.no_pal:
    move.w d0,(a0)
    
    
    
    addq.b #1,.frame_count
    addq.w #4,.sine_index
    andi.w #$3fe,.sine_index
    rts
    
    
    
.spritetext:
    db "SPRITE TEST"
.SPRITE_COUNT = *-.spritetext
    
    
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;
    align 1
mode_scroll_test:
    
    pushsection
    offset
    align 4
.scroll_tbl:
    dcb.w 224
    
.test_num:
    db 0
.next_timer:
    db 0
    
.sine_index:
    dw 0
    
    popsection
    
    
    
.test_intro:
    bsr full_init_vdp
    
    
    moveq #0,d0
    move.b .test_num,d0
    lsl.l #2,d0
    lea .name_tbl,a2
    movea.l (a2,d0),a2
    move.l #BGABASE+(SCREENWIDTH*2*(28/2 - 3))+18,d0
    bsr output_string_normal
    
    
    move.b #$40,.next_timer
    move.l #.test_intro_main,vblank_mode_ptr
    rts
    
    
.test_intro_main:
    SetVDPReg $64,1
    
    subi.b #1,.next_timer
    bne .no_test_init
    move.l #.test_init,vblank_mode_ptr
.no_test_init:
    rts
    
    
    
    
    
.test_init:
    bsr init_vdp_regs
    bsr clear_vram
    
    bsr upload_secret
    
    
    moveq #0,d0
    
    move.w #$8b00,d1
    move.l d0,d2
    move.b .test_num,d2
    lea .scroll_reg_tbl,a2
    move.b (a2,d2),d1
    move.w d1,(a0)
    
    
    lea .scroll_tbl,a2
    moveq #(224*2)/4 - 1,d7
.buf_clear_loop:
    move.l d0,(a2)+
    dbra d7,.buf_clear_loop
    
    move.w d0,.sine_index
    
    move.l #.test_main,vblank_mode_ptr
    rts
    
    
.test_main:
    bsr init_vdp_transfer
    
    
    ;upload scroll
    moveq #0,d0
    lea .scroll_tbl,a2
    
    moveq #0,d6
    move.b .test_num,d2
    move.b d2,d6
    add.l d6,d6
    lea .scroll_shift_tbl,a3
    move.w (a3,d6),d7
    
    cmpi.b #3,d2
    bhs .upload_v
.upload_h:
    SetVDPAddrA HSCROLLBASE,VRAM_WRITE, (a0)
    move.w d7,d6 ;keep the word count so we can recall it later
.upload_h_loop:
    move.w (a2)+,(a1)
    move.w d0,(a1)
    dbra d7,.upload_h_loop
    bra .after_scroll_upload
    
.upload_v:
    SetVDPAddrA 0,VSRAM_WRITE,(a0)
    move.w d7,d6
.upload_v_loop:
    move.w (a2)+,(a1)
    move.w d0,(a1)
    addq.l #2,a2
    dbra d7,.upload_v_loop
    add.w d6,d6

.after_scroll_upload:
    
    
    
    ;shift the scroll buffer back
    move.l d6,d7
    lea .scroll_tbl,a2
    lea 2(a2),a3
    subi.l #1,d6
    bmi .skip_shift
.scroll_shift_loop:
    move.w (a3)+,(a2)+
    dbra d6,.scroll_shift_loop
    
.skip_shift:
    ;now a2 contains the address of the last word of the scroll table
    ;get the new scroll value
    lea sine_tbl,a3
    moveq #0,d0
    moveq #0,d1
    move.w .sine_index,d1
    cmpi.w #$0400,d1
    bhs .set_scroll
    andi.w #$03fe,d1
    move.w (a3,d1),d0
    asr.w #8,d0
.set_scroll:
    add.l d7,d7 ;index *= 2
    move.w d0,(a2)
    
    
    
    
    
    SetVDPRegA $64,1, (a0)
    
    
    
    move.w .sine_index,d0
    cmpi.w #$0600,d0
    blo .no_next_test
    
.next_test:
    move.b .test_num,d0
    addi.b #1,d0
    cmpi.b #5,d0
    blo .next_test_2
    moveq #0,d0
.next_test_2:
    move.b d0,.test_num
    
    move.l #.test_intro,vblank_mode_ptr
    rts
    
.no_next_test:
    addi.w #2, .sine_index
    rts
    
    
    
.scroll_shift_tbl:
    dw 1 - 1
    dw (224-7) - 1
    dw (224) - 1
    dw 1 - 1
    dw (320/8/2) - 1
    
    
.name_tbl:
    dl .name_0
    dl .name_1
    dl .name_2
    dl .name_3
    dl .name_4
    
.name_0:
    db "HORIZONTAL FULL SCROLL",0
.name_1:
    db "HORIZONTAL CELL SCROLL",0
.name_2:
    db "HORIZONTAL LINE SCROLL",0
.name_3:
    db " VERTICAL FULL SCROLL ",0
.name_4:
    db "VERTICAL 2-CELL SCROLL",0
    
    
.scroll_reg_tbl:
    db $00,$02,$03
    db $00,$04
    
    
    
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;
mode_bitmap_overlay_test:
    
    pushsection
    offset
    align 4
.sine_index:
    dw 0
    
._18:
    db 0
    
    popsection
    
    
    bsr init_vdp_regs
    bsr clear_vram
    
    bsr init_cram_cyclone
    bsr upload_font_cyclone
    
    
    SetVDPRegsA ($16c00-($12000-$c000)) >> 10,13, (9<<3),2, (a0) ;hscroll at $16c00, BG A at $18000
    SetVDPRegsA $a,4, $84,5, (a0) ;BG B at $1a000, SAT at $16800
    SetVDPRegsA ($17000-($12000-$c000))>>10,3, 5,18, (a0) ;window at $17000
    SetVDPRegA 2,11, (a0) ;cell h-scroll
    
    
    SetVDPAddrA $400,CRAM_WRITE,(a0)
    move.l #$40404040,(a1)
    
    
    
    ;initialize bitmap
    SetVDPAddrA $4e00,VRAM_WRITE,(a0)
    move.w #(320*224)/2/2 - 1,d7
    moveq #0,d5 ;pixel row
    moveq #0,d6 ;pixel column
    moveq #0,d0 ;output color
.bitmap_loop:
    move.w d5,d0
    move.w d6,d1
    
    btst #6,d0
    beq .no_flip_row
    eori.w #$3f,d0
.no_flip_row:
    andi.w #$3f,d0
    lsl.w #6,d0
    
    btst #6,d1
    beq .no_flip_col
    eori.w #$3f,d1
.no_flip_col:
    andi.w #$3f,d1
    or.w d1,d0
    lsl.w #4,d0
    move.l d0,(a1)
    
    
.after_bitmap_col:
    addi.w #1,d6
    cmpi.w #320/2,d6
    blo .next_bitmap
    moveq #0,d6
    addi.w #1,d5
.next_bitmap:
    dbra d7,.bitmap_loop
    
    
    ;initialize window
    move.l #$17000+(SCREENWIDTH*2*2)+34,d0
    lea .windowtext,a2
    bsr output_string_normal
    
    
    ;initialize backgrounds
    SetVDPRegA SCREENWIDTH*2,15, (a0)
    
    move.l #$18000+(SCREENWIDTH*2*10)+10,d0
    lea .bgatext,a2
    bsr output_string_normal
    
    move.l #$1a000+(SCREENWIDTH*2*10)+70,d0
    lea .bgbtext,a2
    bsr output_string_normal
    
    SetVDPRegA 2,15, (a0)
    
    
    
    move.l #.main,vblank_mode_ptr
    rts
    
    
    
.main:
    bsr init_vdp_transfer
    
    
    lea sine_tbl, a2
    
    ;write SAT
    lea .sprite_tbl,a3
    moveq #0,d6
    move.w .sine_index,d6
    SetVDPAddrA $16800,VRAM_WRITE, (a0)
    moveq #.SPRITE_COUNT - 1,d7
.sprite_loop:
    move.w (a3)+,d0
    move.w (a2,d6),d1
    asr.w #8,d1
    asr.w #3,d1
    add.w d1,d0
    move.w d0,(a1)
    addi.w #$0064,d6
    andi.w #$03fe,d6
    
    move.l (a3)+,(a1)
    move.w (a3)+,(a1)
    dbra d7,.sprite_loop
    
    
    
    ;write background scroll
    moveq #0,d6
    move.w .sine_index,d6
    move.l #$16c00,d7
.scroll_loop:
    move.l d7,d0
    ConvVDPAddr VRAM_WRITE,d0
    move.l d0,(a0)
    
    move.w (a2,d6),d0
    asr.w #8,d0
    asr.w #4,d0
    move.w d0,(a1)
    neg.w d0
    move.w d0,(a1)
    addi.w #$0064,d6
    andi.w #$03fe,d6
    
    
    addi.l #(8*2*2),d7
    cmpi.l #$16c00 + (28*8*2*2),d7
    blo .scroll_loop
    
    
    
    ;do register $18
    move.b ._18,d0
    move.b joy,d1
    
    moveq #BTN_S-1,d7 ;joy bit to test
._18_loop:
    btst d7,d1
    beq ._18_next
    bchg d7,d0
._18_next:
    dbra d7,._18_loop
    
    move.b d0,._18
    move.l #$81649800,d1
    move.b d0,d1
    move.l d1,(a0)
    
    
    
    
    addi.w #$a,.sine_index
    andi.w #$3fe,.sine_index
    
    rts
    
    
    
    
    
    
.SPRITE_COUNT = 7
.sprite_tbl:
    dw $0c0,$01,'S',$104
    dw $0c2,$02,'p',$10c
    dw $0c4,$03,'r',$114
    dw $0c6,$04,'i',$11c
    dw $0c8,$05,'t',$124
    dw $0ca,$06,'e',$12c
    dw $0cc,$00,'s',$134
    
.bgatext:
    db "BACKGROUND A",0
.bgbtext:
    db "BACKGROUND B",0
.windowtext:
    db "WINDOW",0
    
    
    
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;
    align 1
mode_secret:
    bsr init_vdp_regs
    bsr clear_vram
    
    bsr upload_secret
    
    
    
    
    move.l #mode_screenon,vblank_mode_ptr
    rts
    
    
    
    
    
    
upload_secret:
    bsr init_vdp_transfer
    SetVDPRegsA ($c000>>10),2, ($e000>>13),4, (a0)
    SetVDPRegA $80,$18, (a0)
    
    
    
    SetVDPRegsA $34,1, (secret_tiles_size>>2)&$ff,19, (a0)
    SetVDPRegsA (secret_tiles_size>>10)&$ff,20, (secret_tiles>>1)&$ff,21, (a0)
    SetVDPRegsA (secret_tiles>>9)&$ff,22, (secret_tiles>>17)&$7f,23, (a0)
    SetVDPAddrA 0,VRAM_DMA, (a0)
    bsr wait_vdp_dma
    SetVDPRegA $24,1, (a0)
    
    
    SetVDPAddrA $c000,VRAM_WRITE,(a0)
    move.w #($3000/2)-1,d7
    move.w #secret_blank_tile,d0
.bgb_loop:
    move.w d0,(a1)
    dbra d7,.bgb_loop
    
    
    move.l #$c000, d7
    lea secret_tilemap,a2
.tilemap_loop:
    move.l d7,d0
    ConvVDPAddr VRAM_WRITE,d0
    move.l d0,(a0)
    move.w #(40*2/4)-1,d6
.tilemap_row_loop:
    move.l (a2)+,(a1)
    dbra d6,.tilemap_row_loop
    addi.l #SCREENWIDTH*2,d7
    cmpi.l #$c000 + (SCREENWIDTH*2*28),d7
    blo .tilemap_loop
    
    
    
    SetVDPRegsA $34,1, ($100>>1)&$ff,19, (a0)
    SetVDPRegsA ($100>>9)&$ff,20, (secret_palette>>1)&$ff,21, (a0)
    SetVDPRegsA (secret_palette>>9)&$ff,22, (secret_palette>>17)&$7f,23, (a0)
    SetVDPAddrA 0,CRAM_DMA, (a0)
    bsr wait_vdp_dma
    SetVDPRegA $24,1, (a0)
    rts
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; large binary blobs go here
    align 1
sine_tbl:
    incbin "sinetbl"
    
font_data:
    incbin "font.rom",0,8*$80
    
    align 1
secret_tiles:
    incbin "secret-tiles"
secret_blank_tile = (*-secret_tiles)/$30
    dcb.b $30,0
secret_tiles_size = * - secret_tiles
secret_tilemap:
    incbin "secret-tilemap"
secret_palette:
    incbin "secret-palette"
    
    
    
    
    align 1
    dcb.w ($10000-*)/2,$4afc
rom_end: