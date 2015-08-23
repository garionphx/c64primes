
.fileopt comment, "Loader install code portion"
.fileopt compiler, "CA65"
.fileopt author, "Gunnar Ruthenberg"

.include "loader.inc"
.include "../version.inc"

.include "cpu.inc"
.include "cia.inc"
.include "kernal.inc"
.if NONBLOCKING_API
    .include "vic.inc"
.endif

.importzp BLOCKDESTLO

.importzp MAXTRACK41
.importzp MAXTRACK71

.segment "EXTZP"; not used otherwise, the EXTZP segment is not
                ; optional in the o65 built-in ld65 config

.ifdef DYNLINK
    .segment "JUMPTABLE"
    .segment "DATA"

    .macro DYNLINKEXPORT function, label
        .byte function
        .word -1; don't import now, only regard the function as available
    .endmacro

    .out "Exported dynamic link symbols:"
    .word endinstjmptable - *; table size, i.e., offset to the code
    .word version - endinstjmptable

    .include "install-jumptable.inc"
    .include "loader-jumptable.inc"
endinstjmptable:
.else
    .segment "LOADERINSTALL"

    .ifdef INSTADDR
            .org INSTADDR - 2
            .word * + 2; load address
    .endif

    .include "install-jumptable.inc"
.endif

            ; unfortunately, scopes must be defined before using them, this is why the install code is moved to after the drive code

.scope cbm1541
drivecode41:
            .include "drivecode1541.s"
.if UNINSTALL_RUNS_DINSTALL
            .export drivecode41 : absolute
.endif
.endscope

.scope cbm1571
drivecode71:
            .include "drivecode1571.s"
.if UNINSTALL_RUNS_DINSTALL
            .export drivecode71 : absolute
.endif
.endscope

.scope cbm1581
drivecode81:
            .include "drivecode1581.s"
.if UNINSTALL_RUNS_DINSTALL
            .export drivecode81 : absolute
.endif
.endscope

.ifdef install
install2:
.else
install:
.endif
.if LOAD_ONCE
            stx namestrpos+$00
            sty namestrpos+$01
.endif

            lda #diskio::status::OK
            ldx #CIA_SERIAL_DATA_IN_INPUT | CIA_SERIAL_CLK_IN_INPUT | CIA_SERIAL_DATA_OUT_INPUT | CIA_SERIAL_CLK_OUT_INPUT | CIA_SERIAL_ATN_OUT_INPUT | CIA_RS232_INPUT | CIA_VIC2_BANK_OUTPUT
            cpx CIA2_DDRA
            clc
            beq isinstalld
            php; i-flag buffer

            ; try the drive as denoted by FA ($ba, current drive) first
            lda FA
            cmp #MIN_DEVICE_NO
            bcc :+
            cmp #MAX_DEVICE_NO+1
            bcc :++
:           lda #MIN_DEVICE_NO; FA does not contain a drive address (MIN_DEVICE_NO..MAX_DEVICE_NO), try MIN_DEVICE_NO first
:           pha
.if PROTOCOL = PROTOCOLS::TWO_BITS_ATN
            ; check if there is more than 1 drive on the bus,
            ; to make sure the 2bit+atn protocol works alright;
            ; this is done via the low-level serial bus routines,
            ; so non-serial bus devices won't respond
            ldx #MIN_DEVICE_NO
:           stx FA
            lda #$00
            sta STATUS
            jsr drvlistn
            jsr READST
            bmi :++
            pla
            eor #%10000000
            bmi :+
            sta FA
            jsr UNLSTN

    .if LOAD_ONCE && LOAD_VIA_KERNAL_FALLBACK
            jsr openfile
    .endif
            plp; i-flag restore
            lda #diskio::status::TOO_MANY_DEVICES
            ldx #diskio::drivetypes::DRIVE_GENERIC
            sec
            rts
:           pha
:           jsr UNLSTN
            ldx FA
            inx
            cpx #MAX_DEVICE_NO+1
            bne :---
.endif

            lda #$00
            sta STATUS
            pla
.if PROTOCOL = PROTOCOLS::TWO_BITS_ATN
            and #%01111111
.endif
            ; find first available drive
            sta FA
:           pha
            jsr drvlistn
            jsr READST
            pha
            jsr UNLSTN
            pla
            bpl :++; drive present
            ; drive not present, try next address
            ldx FA
            inx
            cpx #MAX_DEVICE_NO+1
            bne :+
            ldx #MIN_DEVICE_NO
:           stx FA
            pla
            cmp FA
            bne :--
            plp; i-flag restore
            lda #diskio::status::DEVICE_NOT_PRESENT
            ldx #diskio::drivetypes::DEVICE_NONE
            sec
isinstalld: rts

:           pla

            ; check which model the drive is

            ; check if we have a 1541/70/71 compatible drive
            lda #<$e5c6
            ldx #>$e5c6
            jsr checktype
            cmp #'4'
            beq is1541
            cmp #'7'
            beq is157x

            ; neither 1541, nor 1570, nor 1571
            ; try 1581
            lda #<$a6e9
            ldx #>$a6e9
            jsr checktype
            ldy #diskio::drivetypes::DRIVE_1581
            cmp #'8'
            beq is1581

            ; no compatible drive found
            plp; i-flag restore
            lda #diskio::status::DEVICE_INCOMPATIBLE
            ldx #diskio::drivetypes::DRIVE_GENERIC
.if LOAD_VIA_KERNAL_FALLBACK
            clc; this is not to be regarded as an error
.else
            sec
.endif
            rts

            ; select appropriate drive code
is1541:     ; find out if 1541 or 1541-C, or 1541-II
            lda #<$c002
            ldx #>$c002
            jsr checktype
            ldy #diskio::drivetypes::DRIVE_1541
            cmp #'c'
            bne selectdcod
            ; find out if 1541-C or 1541-II
            lda #<$eaa3
            ldx #>$eaa3
            jsr checktype
            ldy #diskio::drivetypes::DRIVE_1541_C
            cmp #$ff
            bne selectdcod
            iny; diskio::drivetypes::DRIVE_1541_II
            bne selectdcod

            ; find out if 1570 or 1571
is157x:     cpx #'1' | $80
            lda #MAXTRACK41+1
            ldx #OPC_BIT_ABS
            ldy #diskio::drivetypes::DRIVE_1570
            bcc :+
            lda #MAXTRACK71+1
            ldx #OPC_STA_ABS
            iny; diskio::drivetypes::DRIVE_1571
:           sta c1570fix0 - cbm1571::driveprg71 + cbm1571::drivecode71
            sta c1570fix1 - cbm1571::driveprg71 + cbm1571::drivecode71
            sta c1570fix2 - cbm1571::driveprg71 + cbm1571::drivecode71
            stx c1570fix3 - cbm1571::driveprg71 + cbm1571::drivecode71
            stx c1570fix4 - cbm1571::driveprg71 + cbm1571::drivecode71

is1581:
selectdcod: sty drivetype+$01
            ldx dcodeseltb,y
            lda dcodeselt0,x
            sta dcodesel0
            lda dcodeselt1,x
            sta dcodesel1
            lda dcodeselt2,x
            sta dcodesel2
            lda dcodeselt3,x
            sta dcodesel3
            lda dcodeselt4,x
            sta dcodesel4
            lda dcodeselt5,x
            sta dcodesel5
            lda dcodeselt6,x
            sta dcodesel6
            lda dcodeselt7,x
            sta dcodesel7
            lda dcodeselt8,x
            sta dcodesel8

.if LOAD_ONCE
            jsr openfile
.endif
            jsr drvlistn

            ldx #$00
install1:   ldy #$05
:           lda drvrutmw,y
            jsr IECOUT
            dey
            bpl :-

            ldy #$23
loadcopy:
dcodesel0 = *+$01
dcodesel1 = *+$02
            lda a:$00,x
            jsr IECOUT
            inx
dcodesel2 = *+$01
            cpx #$00
            beq :+
            dey
            bne loadcopy
            jsr drvlistn-$03
            clc
            lda #$23
            adc drvrutmw+$02
            sta drvrutmw+$02
            bcc install1
            inc drvrutmw+$01
            bne install1

:           jsr drvlistn-$03
            ldx #$05
:           lda droutrun-$01,x
            jsr IECOUT
            dex
            bne :-
            jsr UNLSTN

            lda #VIC2_BANK_MASK
            and CIA2_PRA
            sta CIA2_PRA
:           bit CIA2_PRA
            bvs :-
            lda #CIA_SERIAL_DATA_IN_INPUT | CIA_SERIAL_CLK_IN_INPUT | CIA_SERIAL_DATA_OUT_INPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_RS232_OUTPUT | CIA_VIC2_BANK_OUTPUT
            sta CIA2_DDRA
:           bit CIA2_PRA
            bvc :-

dcodesel3 = *+$01
            ldx #$00
dcodesel4 = *+$02
fastinst:   lda a:$00,x
            ldy #$08
            lsr
            nop
            nop
            pha
            lda CIA2_DDRA
            and #~CIA_SERIAL_DATA_OUT_OUTPUT
            bcs :+
            ora #CIA_SERIAL_DATA_OUT_OUTPUT
:           eor #CIA_SERIAL_CLK_OUT_OUTPUT
            sta CIA2_DDRA
            pla
            dey
            bne fastinst+$05
            inx
            bne :+
            inc fastinst+$02
:           cpx dcodesel0
            bne fastinst
            ldy fastinst+$02
            cpy dcodesel1
            bne fastinst

:           bit CIA2_PRA
            bvs :-
            lda #CIA_SERIAL_DATA_IN_INPUT | CIA_SERIAL_CLK_IN_INPUT | CIA_SERIAL_DATA_OUT_INPUT | CIA_SERIAL_CLK_OUT_INPUT | CIA_SERIAL_ATN_OUT_INPUT | CIA_RS232_INPUT | CIA_VIC2_BANK_OUTPUT
            sta CIA2_DDRA
            plp; i-flag restore
            lda #diskio::status::OK
drivetype:  ldx #$00
            ldy #.lobyte(version)
            sty BLOCKDESTLO + 0
            ldy #.hibyte(version)
            sty BLOCKDESTLO + 1
            ldy #BLOCKDESTLO
            clc
            rts

.if NONBLOCKING_API
    .ifdef installnb
installnb2:
    .else
installnb:
    .endif
            ; in: a - nmi line mod 8
            sec
            sbc #$01
            and #%00000111
            sta loadstatus
            jsr install

    .if LOAD_VIA_KERNAL_FALLBACK
            sta initstatus
            bcc :+
            cmp #diskio::status::DEVICE_INCOMPATIBLE
            beq :+
        .if PROTOCOL = PROTOCOLS::TWO_BITS_ATN
            cmp #diskio::status::TOO_MANY_DEVICES
            bne piniterr
        .endif
:
    .else
            bcs piniterr
    .endif
            txa
            pha

            ; disable cia timer nmis
            lda #CIA_CLR_INTF | $7f
            sta CIA2_ICR
            bit CIA2_ICR
            php
            ldx #$00
:           lda VIC2_RASTERLINE
:           cmp VIC2_RASTERLINE
            bne :-
            cmp loadstatus
            beq :+
            dex
            bne :--; this is done so the irq is blocked as late and as short as possible

:           sei
            lda loadstatus
:           cmp VIC2_RASTERLINE
            beq :-
:           cmp VIC2_RASTERLINE; the timer is started at the approximate beginning of a raster line
            bne :-
            lda #.lobyte(CYCLES_PER_LINE_PAL * 8 - $01); timing interval is 8 scanlines XXX TODO ntsc
            sta CIA2_TA_LO
            lda #.hibyte(CYCLES_PER_LINE_PAL * 8 - $01)
            sta CIA2_TA_HI
            lda #$00
            sta CIA2_TB_LO
            sta CIA2_TB_HI
            lda #FORCE_LOAD | TIMER_START
            sta CIA2_CRA
            lda #COUNT_TA_UNDF | FORCE_LOAD | TIMER_START
            sta CIA2_CRB
            plp
            pla
            tax
    .if LOAD_VIA_KERNAL_FALLBACK
            lda #diskio::status::OK
            sta loadstatus
initstatus = *+$01
            lda #diskio::status::OK
            cmp #diskio::status::OK + 1
            rts
    .else
            clc
            lda #diskio::status::OK
    .endif
piniterr:   sta loadstatus
            rts

.endif; NONBLOCKING_API

            jsr UNLSTN
drvlistn:   lda FA
            jsr LISTEN
            lda #SA_OPENCHANNEL | COMMAND_ERROR_CHANNEL
            jmp LSTNSA

checktype:  sta drvchkmr+$03
            stx drvchkmr+$04
            lda #drvrutmw-drvchkmr
            ldx #<drvchkmr
            ldy #>drvchkmr
            jsr SETNAM
            lda #COMMAND_ERROR_CHANNEL
            ldx FA
            tay
            jsr SETLFS
            jsr OPEN
            bcc :+
kernalerr:  pla
            pla
            plp; i-flag restore
            lda #diskio::status::GENERIC_KERNAL_ERROR
            ldx #diskio::drivetypes::DEVICE_UNKNOWN
            sec
            rts
:           ldx #COMMAND_ERROR_CHANNEL
            jsr CHKIN
            jsr CHRIN
            pha
            jsr CHRIN
            pha
            lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jsr CLRCHN
            pla
            tax
            pla
            clc
            rts

.if LOAD_ONCE
openfile:   lda #$02
            ldx FA
            ldy #$00
            jsr SETLFS
            ldx #$ff
:           inx
namestrpos = *+$01
            lda a:$00,x
            bne :-
            txa
            ldx namestrpos+$00
            ldy namestrpos+$01

            jsr SETNAM
            jsr OPEN
            bcs jpkernale

            ldx #$02
            jsr CHKIN

            lda #COMMAND_ERROR_CHANNEL
            ldx FA
            tay
            jsr SETLFS
            lda #$00
            jsr SETNAM
            jsr OPEN
            bcc :+
            lda #$02
            jsr CLOSE
jpkernale:  jmp kernalerr
:
            ldx #COMMAND_ERROR_CHANNEL
            jsr CHKIN
            jsr CHRIN
            cmp #'0'
            bne fileerror
            jsr CHRIN
            cmp #'0'
            beq :+
fileerror:  jsr CHRIN
            cmp #$0d
            bne fileerror

            pla
            pla
            lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            lda #$02
            jsr CLOSE
            jsr CLRCHN
            lda #diskio::status::FILE_NOT_FOUND
            ldx drivetype+$01
            plp; i-flag restore
            sec
            rts
:           lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jmp CLRCHN

.endif; LOAD_ONCE

drvchkmr:   .byte "m-r",$00,$00,$02
dcodesel5 = *+$01
dcodesel6 = *+$02
drvrutmw:   .byte $23,$00,$00,"w-m"
dcodesel7 = *+$00
dcodesel8 = *+$01
droutrun:   .byte $00,$00,"e-m"

dcodeseltb: .byte $00,$00,$00; drivecode1541 for 1541, 1541-C, 1541-II
            .byte $01,$01    ; drivecode1571 for 1570, 1571
            .byte $02        ; drivecode1581 for 1581

dcodeselt0: .byte <(cbm1541::drivebusy41 - cbm1541::driveprg41 + cbm1541::drivecode41)
            .byte <(cbm1571::drivebusy71 - cbm1571::driveprg71 + cbm1571::drivecode71)
            .byte <(cbm1581::drivebusy81 - cbm1581::driveprg81 + cbm1581::drivecode81)
dcodeselt1: .byte >(cbm1541::drivebusy41 - cbm1541::driveprg41 + cbm1541::drivecode41)
            .byte >(cbm1571::drivebusy71 - cbm1571::driveprg71 + cbm1571::drivecode71)
            .byte >(cbm1581::drivebusy81 - cbm1581::driveprg81 + cbm1581::drivecode81)
dcodeselt2: .byte <(cbm1541::drvprgend - cbm1541::drivebusy41)
            .byte <(cbm1571::drvprgend - cbm1571::drivebusy71)
            .byte <(cbm1581::drvprgend - cbm1581::drivebusy81)
dcodeselt3: .byte <cbm1541::drivecode41
            .byte <cbm1571::drivecode71
            .byte <cbm1581::drivecode81
dcodeselt4: .byte >cbm1541::drivecode41
            .byte >cbm1571::drivecode71
            .byte >cbm1581::drivecode81
dcodeselt5: .byte >cbm1541::drivebusy41
            .byte >cbm1571::drivebusy71
            .byte >cbm1581::drivebusy81
dcodeselt6: .byte <cbm1541::drivebusy41
            .byte <cbm1571::drivebusy71
            .byte <cbm1581::drivebusy81
dcodeselt7: .byte >cbm1541::dinstall
            .byte >cbm1571::dinstall
            .byte >cbm1581::dinstall
dcodeselt8: .byte <cbm1541::dinstall
            .byte <cbm1571::dinstall
            .byte <cbm1581::dinstall

version:    .asciiz "Krill's loader, version ", REPOSITORY_VERSION

.ifndef DYNLINK
    .assert * < $d000, error, "***** Error: the install code must not exceed $d000, please make sure the LOADERINSTALL segment ends below $d000. *****"
.endif
