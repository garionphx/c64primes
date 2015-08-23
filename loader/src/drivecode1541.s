
.ifdef STANDALONE
    .include "loader.inc"
.endif; STANDALONE

.include "cpu.inc"
.include "via.inc"

JOBCODE0300          = $00
JOBCODE0400          = $01
JOBCODE0500          = $02
JOBCODE0600          = $03
JOBCODE0700          = $04
JOBCODE0800          = $05
DISKCHANGEBUFFER     = $06
GCRBUFFER            = $07; $07 bytes
CURSTPSL             = $07; these two bytes are only clobbered between
TRACKINC             = $08; fetching and decoding blocks
LINKSECTOR           = $0e
LOADEDSECTOR         = $0f
LINKTRACK            = $10
SENDGCRTABLE         = $11
BLOCKINDEX           = $11
NUMSECTORS           = $12
ID0                  = $16; = ROMOS_HEADER_ID0
ID1                  = $17; = ROMOS_HEADER_ID1
CHAR0BUF             = $18; = ROMOS_HEADER_TRACK
CHAR1BUF             = $19; = ROMOS_HEADER_SECTOR
;GCR_8               = $1a
;GCR_0               = $1b
;GCR_1               = $1c
BLOCKINDEXBASE       = $1d
;GCR_C               = $1e
;GCR_4               = $1f
;GCR_5               = $20
NEXTSECTOR           = $21
SECTORTOFETCH        = $22
;GCR_2               = $23
;GCR_3               = $24
SECTORCOUNT          = $25
;GCR_F               = $26
;GCR_7               = $27
;GCR_8               = $28
CURTRACK             = $29
;GCR_9               = $2a
;GCR_A               = $2b
;GCR_B               = $2c
REQUESTEDSECTOR      = $2d
;GCR_D               = $2e
;GCR_E               = $2f

TRACKLINKTAB         = $30; $15 bytes

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (LOAD_ONCE = 0)
DIRCYCLECOUNT        = $13
CYCLESTARTENDSECTOR  = $14
CURRDIRBLOCKSECTOR   = $15

HASHVALUE0LO         = $30
HASHVALUE0HI         = $31
HASHVALUE1LO         = $32
HASHVALUE1HI         = $33
FILENAME             = $34; $11 bytes

DIRBLOCKPOS          = $45
NEXTDIRBLOCKTRACK    = $46
NEXTDIRBLOCKSECTOR   = $47
NUMFILES             = $48
DDIRBUFF             = $49

DIRBUFFSIZE          = (DISKCHANGEFLAG - DDIRBUFF) / 4;
DIRTRACKS            = DDIRBUFF
DIRSECTORS           = DIRTRACKS + DIRBUFFSIZE
FILENAMEHASHVAL0     = DIRSECTORS + DIRBUFFSIZE
FILENAMEHASHVAL1     = FILENAMEHASHVAL0 + DIRBUFFSIZE
DIRBUFFEND           = FILENAMEHASHVAL1 + DIRBUFFSIZE

.assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"
.endif; (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (LOAD_ONCE = 0)

.if UNINSTALL_RUNS_DINSTALL
DINSTALLBUFFER       = DISKCHANGEFLAG - drvprgend + dinstall
.endif


HINIBBLES            = $0600
LONIBBLES            = $0700
SECTORLINKTAB        = $0780
TEMPTRACKLINKTAB     = $07c0

GETDRIVECODE         = $07e6

ROMOS_HEADER_ID0     = $16
ROMOS_HEADER_ID1     = $17
ROMOS_HEADER_TRACK   = $18
ROMOS_HEADER_SECTOR  = $19

GCRENCODE            = $f77f
GCRDECODEHI          = $f8a0
GCRDECODELO          = $f8c0


BINARY_NIBBLE_MASK   = %00001111
GCR_NIBBLE_MASK      = %00011111

NUMMAXSECTORS        = 21
MAXTRACK41           = 41

JOBCODE_EXECUTE      = $d0

ANYSECTOR            = $40
UNPROCESSEDSECTOR    = $ff


.if !UNINSTALL_RUNS_DINSTALL
    ORG = $0032
.else
    ORG = $0007
.endif

.if UNINSTALL_RUNS_DINSTALL
    .export driveprg41  : absolute
    .export drivebusy41 : absolute
.endif; UNINSTALL_RUNS_DINSTALL

.export MAXTRACK41


.ifdef STANDALONE

            .org ORG - 1
            .word * + 2
.else
            .org ORG

driveprg41: .byte .hibyte(drivebusy41 - * + $0100 - $01); init transfer count hi-byte

.endif; !STANDALONE

dcodinit:   jsr drivebusy41

            lda #T1_IRQ_ON_LOAD | PA_LATCHING_ENABLE; watchdog irq: count phi2 pulses, one-shot;
                                                    ; enable port a latching to grab 1 gcr byte at a time
                                                    ; rather than letting the gcr bitstream scroll through
                                                    ; port a (applies to 1541-I and Oceanic OC-118, but not
                                                    ; 1541-II)
            sta VIA2_ACR
            lda #READ_MODE | BYTE_SYNC_ENABLE
            sta VIA2_PCR

.if UNINSTALL_RUNS_DINSTALL
            lda JOBCODE0300
            sta ID0
    .assert * >= ID0, error, "***** 'ID0' written before executed. *****"
            lda JOBCODE0400
            sta ID1
    .assert * >= ID1, error, "***** 'ID1' written before executed. *****"
            lda JOBCODE0500
            sta CHAR0BUF
    .assert * >= CHAR0BUF, error, "***** 'CHAR0BUF' written before executed. *****"
            lda JOBCODE0600
            sta CHAR1BUF
    .assert * >= CHAR1BUF, error, "***** 'CHAR1BUF' written before executed. *****"
.else
            ; before loading the first file, the current track number is
            ; retrieved by reading any block header on the disk -
            ; however, if the loader is uninstalled before loading anything,
            ; it needs the more or less correct current track number to
            ; seek to track 18
            lda ROMOS_HEADER_TRACK
            sta CURTRACK
.endif

            ; watchdog initialization
.if !UNINSTALL_RUNS_DINSTALL
            lda #IRQ_CLEAR_FLAGS | $7f
            sta VIA1_IER; no irqs from via 1
            sta VIA2_IER; no irqs from via 2
.endif
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
            sta VIA2_IER; timer 1 irqs from via 2
           ;lda #$00
            ldx #$04
:           sta JOBCODE0400,x; clear job queue
            dex
            bpl :-
            lda #JOBCODE_EXECUTE
            sta JOBCODE0300; execute watchdog handler at $0300 on watchdog time-out

            ldy #$0f

    .assert * >= SENDGCRTABLE + $1f, error, "***** 'mkgcrdec' overwrites itself. *****"

mkgcrdec:   lda sendgcrraw,y
            ldx GCRENCODE,y
            sta SENDGCRTABLE,x
            dey
            bpl mkgcrdec

            ; before spinning up the motor and finding the current track,
            ; wait until a file is requested to be loaded at all
:           lda VIA1_PRB
            cmp #CLK_OUT | CLK_IN | DATA_IN | ATN_IN
            beq :-; wait until there is something to do

            cmp #CLK_OUT | CLK_IN | DATA_IN
            beq :+
            ldx #$00; don't fade off led because it's not lit
            jmp duninstall; check for reset or uninstallation
:
            lda VIA2_PRB
            ora #MOTOR
            and #~BITRATE_MASK; reset bit-rate
            sta VIA2_PRB
            and #WRITE_PROTECT
            sta DISKCHANGEBUFFER; store light sensor state for disk removal detection

            ldx #NUMMAXSECTORS
            stx NUMSECTORS

            ; find current track number
            ; this assumes the head is on a valid half track
:           clc
            lda #1 << BITRATE_SHIFT
            adc VIA2_PRB
            sta VIA2_PRB; cycle through the 4 bit-rates

            lda #-$01; invalid track number -> no track step
            sta CURTRACK
            ldy #ANYSECTOR
            jsr getblkstid; sector link sanity check is disabled here
            bcs :-

            lda GCRBUFFER+$04
            asl GCRBUFFER+$03
            rol
            asl GCRBUFFER+$03
            rol
            asl GCRBUFFER+$03
            rol
            and #GCR_NIBBLE_MASK
            tay
            lda GCRBUFFER+$03
            jsr decodesub; track
            cmp CURTRACK; getblkstid sets CURTRACK at this stage
            bne :-

            lda #OPC_EOR_ZP
            sta headerchk-$02
            lda #OPC_BNE
            sta headerchk
            lda #OPC_STA_ABS
            sta putbitrate
            lda #OPC_STX_ZP
            sta putnumscts
            jmp beginload


sendgcrraw:
            BIT0DEST = 3
            BIT1DEST = 1
            BIT2DEST = 2
            BIT3DEST = 0

            .repeat $10, I
                .byte (((~I >> 0) & 1) << BIT0DEST) | (((~I >> 1) & 1) << BIT1DEST) | (((~I >> 2) & 1) << BIT2DEST) | (((~I >> 3) & 1) << BIT3DEST)
            .endrep

DISKCHANGEFLAG:
            .word newdisk

.if UNINSTALL_RUNS_DINSTALL

runcodeget: ldx #getdrcodee - getdrvcode - $01
:           lda .lobyte(getdrvcode),x
            sta GETDRIVECODE,x
            dex
            bpl :-
            jmp GETDRIVECODE

getdrvcode: ldx #.lobyte(stackend-$03)
            txs
            ldx #.lobyte(driveprg41-$01)
getroutine: inx
            bne :+
            inc getroutputhi - getdrvcode + GETDRIVECODE
:           jsr dgetbyte
getroutputhi = *+$02
            sta a:.hibyte(driveprg41-$01) << 8,x
            cpx #.lobyte(drivebusy41-$01)
            bne getroutine
            dec driveprg41
            bne getroutine
            rts; jumps to dcodinit
getdrcodee:

.endif; UNINSTALL_RUNS_DINSTALL

            ; common code for all configurations

            ; $0a bytes
checkchg:   lda VIA2_PRB
            and #WRITE_PROTECT
            cmp DISKCHANGEBUFFER
            sta DISKCHANGEBUFFER
            rts

            ; $17 bytes
gcrencode:  pha
            and #BINARY_NIBBLE_MASK
            tax
            lda GCRENCODE,x
            sta LONIBBLES,y
            pla
            lsr
            lsr
            lsr
            lsr
            tax
            lda GCRENCODE,x
            sta HINIBBLES,y
            rts

          ; * = $0100
stack:
    .assert stack = $0100, error, "***** Stack not located at $0100. *****"

            ; stack frame
.if LOAD_ONCE
            .res 9; padding, best used for bigger stack
.endif
            .res 5; padding, best used for bigger stack
            .word $00, $00, $00, dcodinit-$01
stackend:

            ; $17 bytes
waitsync:   bit VIA2_T1C_H
            bpl wsynctmout; will return $00 in the accu
            bit VIA2_PRB
            bmi waitsync
            bit VIA2_PRA
            clv
            bvc *
            ldx #$00
            lda VIA2_PRA; is never $00 but usually $52 (header) or $55 (data)
            clv
            rts

            ; $0d bytes
gcrtimeout: jsr checkchg; check light sensor for disk removal
            beq :+
            lda #.lobyte(newdisk)
            sta DISKCHANGEFLAG; set the new disk flag when disks have been
                              ; changed
:           sec

            ; fall through

wsynctmout: lda #%00000000
            rts

getblock:   sta dsctcmps
            lda CURTRACK
.if LOAD_ONCE
            ; get the block at track a, sector y, check against stored id
getblkchid:
getblkstid: ldx #OPC_CMP_ZP
.else
            ; get the block at track a, sector y, check against stored id
getblkchid: ldx #OPC_CMP_ZP
            SKIPWORD
            ; get the block at track a, sector y, store read id
getblkstid: ldx #OPC_STA_ZP
.endif
            stx storputid1
            dex; OPC_STA_ZP/OPC_CMP_ZP -> OPC_STY_ZP/OPC_CPY_ZP
            stx storputid0
            sty REQUESTEDSECTOR
            jsr trackseek
            ; x contains the number of blocks on the current track here

            lda #OPC_BNE; full gcr fetch and checksumming
wait4sct:   sta scanswt0
            sta scanswt1

            lda #$ff
            sta VIA2_T1C_H; reset the watchdog timer

readblock:  jsr waitsync
            beq gcrtimeout; returns with carry set on time-out
            cmp #%01010010; check if the sync is followed by a sector header
            bne readblock; if not, wait for next sync mark

            ; read the sector header
            ldx #$06
getheader:  bvc *
            lda VIA2_PRA
            clv
            sta GCRBUFFER+$00,x
            dex
            bpl getheader

            ; check if the sector header's field values match the expectations -
            ; the header is only checksummed after the data fetch since there
            ; is not enough time to do it now

            ; decode sector number
            lda GCRBUFFER+$04
            asl
            tax
            lda GCRBUFFER+$05
            rol
            and #GCR_NIBBLE_MASK
            tay
            txa
            jsr decodesub+$00
            cmp NUMSECTORS; check if sector number is within range of the allowed
                          ; sector numbers for the current track
            bcs readblock
            sta LOADEDSECTOR; store away the sector number, it is returned in the
                            ; x-register on success
            tax             ; current sector number

            lda REQUESTEDSECTOR; bit:bmi:bvs won't work because of
            asl                ; the BYTESYNC signal in the v-flag
            bcs checkprocd; branch if UNPROCESSEDSECTOR
            bmi waitdatahd; branch if ANYSECTOR

            cpx REQUESTEDSECTOR
            beq waitdatahd
            bne readblock ; jmp

            ; normal operation and no specific sector requested -
            ; out-of-order sector fetch
checkprocd: lda TRACKLINKTAB,x; check whether the current block has already been
                              ; loaded into the computer's memory
            bmi readblock; if yes, wait for next sector

            sta BLOCKINDEX; store sector index

            ; wait for data block sync
waitdatahd: lda #$ff
            sta VIA2_T1C_H
            jsr waitsync
            cmp #%01010101; check if the sync is followed by a data block
            bne readblock; if not, wait for next sector

            ; read and partially inflate the gcr digits to 8 bits
           ;ldx #$00
loaddata:   bvc *
            lsr                  ; .0000011
            sta HINIBBLES-$01-0,x
            lda VIA2_PRA         ; 11222223      0 - cycle 13
            ror                  ; 11122222
            sta LONIBBLES-$01-0,x
            and #GCR_NIBBLE_MASK ; ...22222 - 2
            sta HINIBBLES+$00-0,x
            inx
            inx
            lda VIA2_PRA         ; 33334444      1 - cycle 35
            clv                  ;                 - cycle 37
            tay
            ror                  ; 33333444
            lsr                  ; .3333344
            lsr                  ; ..333334
            lsr                  ; ...33333 - 3
            sta LONIBBLES+$00-2,x
               ; 52 cycles

            bvc *
            tya
            ldy VIA2_PRA         ; 45555566      2 - cycle 8
            cpy #$80
            rol                  ; 33344444
            and #GCR_NIBBLE_MASK ; ...44444 - 4
            sta HINIBBLES+$01-2,x
            tya                  ; 45555566
            alr #%01111111       ; ..555556
            sta LONIBBLES+$01-2,x
            inx
            nop
            lda VIA2_PRA         ; 66677777      3 - cycle 36
            tay
            ror                  ; 66667777
            lsr LONIBBLES+$01-3,x; ...55555 - 5
            ror                  ; 66666777
            sta HINIBBLES+$02-3,x
            tya                  ; 66677777
            and #GCR_NIBBLE_MASK ; ...77777 - 7
            sta LONIBBLES+$02-3,x
            lda VIA2_PRA         ; 00000111      4 - cycle 65
            inx
            clv                  ;                 - cycle 69
scanswt0:   bne loaddata
               ; 72 cycles

    .assert .hibyte(* + 1) = .hibyte(loaddata), error, "***** Page boundary crossing in GCR fetch loop, fatal cycle loss. *****"

            lsr                  ; .0000011
            sta HINIBBLES+$ff
            bvc *
            clv
            lda VIA2_PRA         ; 11222223
            ror                  ; 11122222
            sta LONIBBLES+$ff
            and #GCR_NIBBLE_MASK ; ...22222 - 2
            tay
            bvc *
            lda VIA2_PRA         ; 33334444
            jsr decodesub-$01    ; decode data checksum
            tay

            ; finish gcr inflation and checksum the data
            ldx #$00
gcrfinish:  lda HINIBBLES+$02,x  ; 66666777
            lsr                  ; .6666677
            lsr                  ; ..666667
            lsr                  ; ...66666 - 6
            sta HINIBBLES+$02,x
            lda LONIBBLES+$03,x  ; 11122222
            lsr HINIBBLES+$03,x  ; ..000001
            ror                  ; 11112222
            lsr HINIBBLES+$03,x  ; ...00000 - 0
            ror                  ; 11111222
            lsr                  ; .1111122
            lsr                  ; ..111112
            lsr                  ; ...11111 - 1
            sta LONIBBLES+$03,x
            tya
            ldy HINIBBLES+$00,x
            eor GCRDECODEHI,y
            ldy LONIBBLES+$00,x
            eor GCRDECODELO,y
            ldy HINIBBLES+$01,x
            eor GCRDECODEHI,y
            ldy LONIBBLES+$01,x
            eor GCRDECODELO,y
            ldy HINIBBLES+$02,x
            eor GCRDECODEHI,y
            ldy LONIBBLES+$02,x
            eor GCRDECODELO,y
            ldy HINIBBLES+$03,x
            eor GCRDECODEHI,y
            ldy LONIBBLES+$03,x
            eor GCRDECODELO,y
            tay
            txa
            axs #-$04; x = x + 4
scanswt1:   bne gcrfinish

    .assert .hibyte(* + 1) = .hibyte(gcrfinish), error, "***** Page boundary crossing in GCR finishing loop, unnecessary cycle loss. *****"

            txa
            bne :+; don't check sum if only the first few bytes have been
                  ; decoded for scanning
            tya
            beq :+; check whether data checksum is ok
jpreadbk:   jmp readblock; if not, wait for next sector
:
            ; checksum sector header
            ; this is done only now because there is no time for that between
            ; the sector header and data block
            lda GCRBUFFER+$06
            alr #(GCR_NIBBLE_MASK << 1) | 1
            tay
            lda GCRBUFFER+$05
            jsr decodesub-$01; check sum
            sta GCRBUFFER+$06
            lax GCRBUFFER+$02
            lsr
            lsr
            lsr
            tay
            txa
            asl GCRBUFFER+$01
            rol
            asl GCRBUFFER+$01
            rol
            and #GCR_NIBBLE_MASK
            jsr decodesub+$03; ID1
            sta GCRBUFFER+$05
            lda GCRBUFFER+$01
            lsr
            lsr
            lsr
            tay
            lda GCRBUFFER+$00
            jsr decodesub-$01; ID0
            tay
            eor GCRBUFFER+$05; ID1
            eor LOADEDSECTOR
            eor GCRBUFFER+$06; check sum
            sta CURTRACK; is changed to eor CURTRACK after init
headerchk:  .byte OPC_BIT_ZP, .lobyte(jpreadbk-*-$01); is changed to bne jpreadbk
                                        ; after init, wait for next sector if
                                        ; sector header checksum was not ok
            lda GCRBUFFER+$05
            ldx #$00; set z-flag which won't be altered by the store opcodes
storputid0: cpy ID0; cpy ID0/sty ID0
            bne :+
storputid1: cmp ID1; cmp ID1/sta ID1
:           clc
dsctcmps:   .byte OPC_RTS, .lobyte(jpreadbk-*-$01); is changed to bne jpreadbk
                                            ; branch if some error occured while
                                            ; checking the sector header

            ; sector link sanity check
            ldy #$00
            jsr decodebyte; decode the block's first byte (track link)
            cmp #MAXTRACK41+1; check whether track link is within the valid range
            bcs jpreadbk; if not, wait for next sector
            sta LINKTRACK
            jsr decodebyte; decode the block's second byte (sector link)
            sta LINKSECTOR; return the loaded block's sector link sector number
            ldy LINKTRACK ; return the loader block's sector link track number
            beq :+
            cmp NUMSECTORS
            bcs jpreadbk  ; branch if sector number too large

:           ldx LOADEDSECTOR; return the loaded block's sector number
           ;clc             ; operation successful
            rts

            ; $19 bytes
lightsub:   txa
            tay
            beq ddliteon-$01
:           adc #$01
            bne :-
            jsr ddliteon
:           dey
            bne :-
            dex
            bne :+
            and #~MOTOR   ; turn off motor
:           and #~BUSY_LED; turn off busy led
store_via2: sta VIA2_PRB
            rts

            ; $07 bytes
ddliteon:   lda #BUSY_LED
            ora VIA2_PRB
            bne store_via2

drividle:   jsr lightsub; fade off the busy led
            jsr gcrtimeout; check light sensor for disk removal
            lda VIA1_PRB
            cmp #CLK_OUT | CLK_IN | DATA_IN | ATN_IN
            beq drividle; wait until there is something to do

            cmp #CLK_OUT | CLK_IN | DATA_IN
            bne duninstall; check for reset or uninstallation

            ; load a file

            txa
            beq beginload; check whether the busy led has been completely faded off
            jsr ddliteon ; if not, turn it on
beginload:
.if !LOAD_ONCE ; not with LOAD_ONCE because then, there is no danger of getting stuck
               ; because there is no serial transfer to retrieve the file id
            lda #$ff
            sta VIA2_T1C_H; set watchdog time-out, this also clears the possibly
                          ; pending timer 1 irq flag
            cli; enable watchdog, the c64 might be reset while sending over a
               ; byte, leaving the drive waiting for handshake pulses
.endif
            bcs getstartts; jmp

    .assert * = $0300, error, "***** 1541 watchdog IRQ handler not located at $0300. *****"

            ; configuration-dependent code

.if UNINSTALL_RUNS_DINSTALL

watchdgirq: lda #BUSY_LED | MOTOR
            jsr ddliteon+$02
            lda #$12
            jsr trackseek; ignore error (should not occur)
            ldx #$ff
            ; fade off the busy led and reset the drive
:           jsr lightsub
            txa
            bne :-
            jmp (RESET_VECTOR)
duninstall:
:           jsr lightsub
            txa
            bne :-
            jmp runcodeget

.else

watchdgirq: ldx #$ff

duninstall: txa
            pha
            bne :+
            lda #MOTOR
            SKIPWORD
:           lda #BUSY_LED | MOTOR
            jsr ddliteon+$02
            lda #$12
            jsr trackseek; ignore error (should not occur)
            pla
            ; fade off the busy led (if lit) and reset the drive
            beq :++
            ldx #$ff
:           jsr lightsub
            txa
            bne :-
:           jmp (RESET_VECTOR)
.endif

            ; $0c bytes
            ror
decodesub:  lsr
            lsr
            lsr
            tax
            lda GCRDECODEHI,y
            ora GCRDECODELO,x
            rts

            ; $10 bytes
decodebyte: ldx HINIBBLES,y
            lda GCRDECODEHI,x
            ldx LONIBBLES,y
            ora GCRDECODELO,x
.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (!LOAD_ONCE)
            ldx NUMFILES
.endif
            iny
            rts

            ; get starting track and sector of the file to load
getstartts:
.if !LOAD_ONCE

    .if ::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME

            ldx #-$01
            stx DIRCYCLECOUNT
getfilenam: inx
            jsr dgetbyte; get filename
            beq :+
            sta FILENAME,x
            cpx #FILENAME_MAXLENGTH
            bne getfilenam
:           jsr drvwait0
            jsr gethashval
            sta CHAR1BUF
            stx CHAR0BUF

    .elseif ::FILESYSTEM = FILESYSTEMS::TRACK_SECTOR

            jsr dgetbyte; get starting track
            sta CHAR0BUF
            jsr dgetbyte; get starting sector
            jsr drvwait0
            sta CHAR1BUF

   .endif
.endif

.if (::FILESYSTEM = FILESYSTEMS::TRACK_SECTOR) || LOAD_ONCE
            ; check for illegal track or sector
            ldy CHAR0BUF
            beq toillegal+$00
            cpy #MAXTRACK41+1
            bcs toillegal+$01
            ldx CHAR1BUF
            cpx NUMSECTORS
            bcc :+
toillegal:  sec
            jmp illegalts
:
.endif
            ; turn on the motor (but do not turn on the busy led yet)
            lda #MOTOR
            jsr ddliteon+$02

spinloop:   lda #OPC_RTS  ; get any block on the current track,
            ldy #ANYSECTOR; no sector link sanity check,
            jsr getblock  ; don't store id
            bcs spinloop; retry until any block has been loaded correctly

            beq :+; branch if disk id is the same, if not, re-read the dir
            lda #.lobyte(newdisk)
            sta DISKCHANGEFLAG; set the new disk flag when disks have been changed
:           lda #OPC_BNE
            sta dsctcmps
            jmp (DISKCHANGEFLAG); jumps to newdisk if a disk change has happened,
                                ; and to samedisk otherwise

newdisk:    ; a new disk has been inserted

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (!LOAD_ONCE)

:           lda #DIRTRACK
            ldy #DIRSECTOR
            jsr getblkstid; sector link sanity check
                          ; store id
            bcs :-
           ;clc
            sta CYCLESTARTENDSECTOR

dirwrap:    sta NEXTDIRBLOCKSECTOR
            sty NEXTDIRBLOCKTRACK
            bcs dnxtdirs
            ; directory cycling: fill the dir buffer
filldirbuf: lda #$00
            sta NUMFILES
dnxtdirs:   jsr checkchg
            bne :-; store disk id and start over if disk changed
            lda NEXTDIRBLOCKTRACK
            bne :+
            ; end of directory, so wrap the cycle
            ; little flaw for the sake of saving on code size:
            ; if there are less files in the directory than the
            ; dir buffer can hold, they are stored multiple
            ; times until the buffer is full. this costs some extra
            ; time but only matters when changing disks
            lda #DIRSECTOR
            ldy #DIRTRACK
           ;sec; carry is set by checkchg
            bcs dirwrap; jmp

:           ldy NEXTDIRBLOCKSECTOR
            jsr getblkchid; compare id, sector link sanity check
            bcs dnxtdirs; retry on error
            sta NEXTDIRBLOCKSECTOR
            stx CURRDIRBLOCKSECTOR
            sty NEXTDIRBLOCKTRACK

            ; when reading the dir, the DIRCYCLECOUNT variable
            ; is increased every time the CYCLESTARTENDSECTOR
            ; is read
            cpx CYCLESTARTENDSECTOR
            bne :+
            inc DIRCYCLECOUNT; on cycle start: $ff->$00

:           ldy #$03
dgdirloop:  ldx NUMFILES
            jsr fnamehash
            pha
            txa
            ldx NUMFILES
            sta FILENAMEHASHVAL0,x
            pla
            sta FILENAMEHASHVAL1,x

            ; there is no check of non-files (start track 0) here
            ; to save some space

            inc NUMFILES
            cpx #DIRBUFFSIZE - 1
            ; branch if dir buffer is full
            ; little flaw for the sake of saving on code size:
            ; when starting to cycle through the directory, the
            ; files in the dir block the last file currently in the dir
            ; buffer is in, will all be added to the buffer when it will
            ; be filled on the subsequent file load - this is why the
            ; minimum dir buffer size is 9 files
            bcs samedisk
            tya
            and #%11100000
           ;clc
            adc #$23
            tay
            bcc dgdirloop; process all entries in a dir block
            bcs dnxtdirs ; check next dir block


            ; the disk was not changed, or the dir has just been read
samedisk:   lda #.lobyte(samedisk); clear new disk flag
            sta DISKCHANGEFLAG

            ldx #-$01
findfile:   inx
            cpx NUMFILES
            bne :+; check all dir entries in the buffer

            ; the dir buffer does not contain the file,
            ; so cycle through the directory to find it

           ;lda #DIRTRACK        ; not needed since NEXTDIRBLOCKTRACK
           ;sta NEXTDIRBLOCKTRACK; is always left holding DIRTRACK

            ldy DIRCYCLECOUNT    ; check if a cycle
            dey                  ; is complete
            bmi filldirbuf       ; the counter will increase on cycle start
            jmp filenotfnd

:           lda CHAR0BUF
            cmp FILENAMEHASHVAL0,x
            bne findfile
            lda CHAR1BUF
            cmp FILENAMEHASHVAL1,x
            bne findfile

            stx CHAR0BUF; store index of file to jump to the track of the file
                        ; following this one in the dir, after loading

            ; store track and sector of the dir block the file was found in,
            ; they are used to start the dir check cycle if the next file is not found
            ; in the dir buffer;
            ; they are also checked on the subsequent load to determine if the dir check
            ; cycle is complete and the file be said to be not found
            lda CURRDIRBLOCKSECTOR
            sta CYCLESTARTENDSECTOR
            sta NEXTDIRBLOCKSECTOR
            lda #DIRTRACK
            sta NEXTDIRBLOCKTRACK

            jsr ddliteon
            lda DIRTRACKS,x
            ldy DIRSECTORS,x

            ; actually there should be a check for illegal track or sector
            ; here - unfortunately, there is no memory left for it

.else; ::FILESYSTEM = FILESYSTEMS::TRACK_SECTOR || (!LOAD_ONCE)

            ; store new disk id
:           lda CURTRACK
            ldy #ANYSECTOR
            jsr getblkstid; sector link sanity check
                          ; store id
            bcs :-

samedisk:   lda #.lobyte(samedisk); clear new disk flag
            sta DISKCHANGEFLAG

            jsr ddliteon
            lda CHAR0BUF; track
            ldy CHAR1BUF; sector
.endif

    .assert .hibyte(samedisk) = .hibyte(newdisk), error, "***** 'newdisk' and 'samedisk' are not in the same memory page. *****"

            ; a contains the file's starting track here
            ; y contains the file's starting sector here
            ldx #$00
            stx BLOCKINDEXBASE
trackloop:  sty SECTORTOFETCH
            jsr trackseek

            ; scan the track for the file links
           ;lda #UNPROCESSEDSECTOR    ; the accu contains that, effectively
:           sta TRACKLINKTAB-$01,x    ; mark all sectors as not processed
            sta TEMPTRACKLINKTAB-$01,x; mark all sectors as not processed
            dex
            bne :-
scantrloop: lda #OPC_BIT_ZP; only fetch the first few bytes to track the links
            ; this is a weak point since there is no data checksumming here
            ldy #ANYSECTOR
            sty REQUESTEDSECTOR
            jsr wait4sct
            bcs scantrloop; branch until fetch successful

            ; x contains the loaded block's sector number here
            sta SECTORLINKTAB,x
            tya
            sta TEMPTRACKLINKTAB,x; store sector's track link and mark the sector as
                                  ; processed

            ; go through the link list to find the blocks's order on the track
            ldy #$00
            sty BLOCKINDEX
            ldx SECTORTOFETCH
numblklp:   lda TEMPTRACKLINKTAB,x
            bmi scantrloop; branch if not all of the file's blocks on this track
                          ; have been scanned yet
            pha; store link track
            tya
            sta TRACKLINKTAB,x ; store sector index
            lda SECTORLINKTAB,x; get link sector
            tax
            iny; increase sector index
            pla
            cmp CURTRACK; check whether link track is the current track
            beq numblklp; branch until all the file's blocks on the current
                        ; track have been ordered

            ; read and transfer all the blocks that belong to the file
            ; on the current track, the blocks are read in quasi-random order
            pha         ; next track
            tya         ; number of the file's blocks on the current track
            pha
            stx NEXTSECTOR; first sector on the next track
            sty SECTORCOUNT; number of the file's blocks on the current track

loadblock:  ldy #UNPROCESSEDSECTOR; find any yet unprocessed block belonging to the file
            lda BLOCKINDEXBASE
            bne :+
            ldy SECTORTOFETCH; on loading begin, first get the file's first block to
                             ; determine its loading address
:           lda #OPC_BNE
            jsr getblock     ; read any of the files's sectors on track, compare
                             ; id, sector link sanity check
            bcs loadblock    ; retry until a block has been successfully loaded

            ; mark the block as processed and send it over
            ldy #UNPROCESSEDSECTOR; $ff
            sty SECTORTOFETCH; BLOCKINDEXBASE is 0 for the whole 1st file track
            sty TRACKLINKTAB,x; mark the loaded block as processed
            lda LINKTRACK
            bne :+
            ldy LINKSECTOR; the file's last block's length

:           sty dsendcmp+$01
            dey
            dey
            tya
            ldy #$01
            jsr gcrencode; block length
            clc
            lda BLOCKINDEX
            adc BLOCKINDEXBASE
            jsr sendblock; send the block over

            lda SECTORCOUNT; sector count for the current track
            bne loadblock
            clc
            pla; number of file's blocks on the current track
            adc BLOCKINDEXBASE
            sta BLOCKINDEXBASE
            ldy NEXTSECTOR
            pla          ; next track
            bne trackloop; process next track

            ; loading is finished

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (!LOAD_ONCE)

            ldx CHAR0BUF
:           inx
            cpx NUMFILES
            lda DIRTRACKS,x
            beq :-; track number might be 0 since non-files may be in the dir buffer as well
            bcc :+
            lda #DIRTRACK
:           jsr trackseek; move head to the start track of the next file in the
                         ; directory
            clc

filenotfnd: ; branches here with carry set on file not found

.else
            clc
.endif

illegalts:  ; or illegal t or s
            lda #$00
            sta dsendcmp+$01
            sbc #$01; carry clear: result is $00-$02=$fe - loading finished successfully
                    ; carry set:   result is $00-$01=$ff - load error
            jsr sendblock; send status

            ldx #$01; turn motor and busy led off
            lda #BUSY_LED; check if busy led is lit
            and VIA2_PRB
            beq :+
            ldx #$ff; fade off the busy led, then turn motor off
.if LOAD_ONCE
:           jmp duninstall
.else
:           cli; enable watchdog
:           bit VIA1_PRB
            bpl :-; wait until the computer has acknowledged the file transfer
            sei; disable watchdog
            jmp drividle
.endif

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (!LOAD_ONCE)

fnamehash:  jsr decodebyte; get file's start track
            sta DIRTRACKS,x
            jsr decodebyte; get file's start sector
            sta DIRSECTORS,x
            ldx #$00
:           stx HASHVALUE0LO
            jsr decodebyte
            ldx HASHVALUE0LO
            cmp #' ' | $80
            beq gethashval
            sta FILENAME,x
            inx
            cpx #FILENAME_MAXLENGTH
            bne :-

gethashval: clc
            lda #$ff
            sta HASHVALUE0LO
            sta HASHVALUE1LO
            lda #$00
            sta HASHVALUE0HI
            sta HASHVALUE1HI
hashloop:   lda FILENAME - 1,x
            adc HASHVALUE0LO
            sta HASHVALUE0LO
            bcc :+
            inc HASHVALUE0HI
            clc
:           adc HASHVALUE1LO
            sta HASHVALUE1LO
            lda HASHVALUE0HI
            adc HASHVALUE1HI
            sta HASHVALUE1HI
            dex
            bne hashloop
            adc HASHVALUE1LO
            tax
            lda HASHVALUE0LO
            adc HASHVALUE0HI
            rts
.endif

trackseek:  cmp #MAXTRACK41+1
            bcs setbitrate; don't do anything if invalid track
            sec
            tax
            sbc CURTRACK
            beq setbitrate

            ; do the track jump

            stx CURTRACK
            ldy #$00
            sty CURSTPSL
            bcs :+
            eor #~$00; invert track difference
            adc #$01
            iny
:           sty TRACKINC
            asl
            tay
            lda #$80 | (MINSTPSP+1)
trackstep:  pha
            sta VIA2_T1C_H
            tax
            lda TRACKINC
            eor VIA2_PRB
            sec
            rol
            and #TRACK_STEP
            eor VIA2_PRB
            sta VIA2_PRB
            pla
headaccl:   cmp #$80 | MAXSTPSP
            beq noheadac
            pha
           ;sec
            lda CURSTPSL
            sbc #STEPRACC
            sta CURSTPSL
            pla
            sbc #$00
noheadac:   cpx VIA2_T1C_H
            beq noheadac; wait until the counter hi-byte has decreased by 1
            dex
            bmi headaccl
            dey
            bne trackstep

            ; bit-rates:
            ; 31+  : 00
            ; 25-30: 01
            ; 18-24: 10
            ;  1-17: 11
setbitrate: lda VIA2_PRB
            ora #SYNC_MARK | BITRATE_MASK
            ldx #$15
            ldy CURTRACK
            cpy #$12
            bcc putbitrate
            dex
            dex
            sbc #1 << BITRATE_SHIFT
            cpy #$19
            bcc putbitrate
            dex
            sbc #1 << BITRATE_SHIFT
            cpy #$1f
            bcc putbitrate
            dex
            sbc #1 << BITRATE_SHIFT
putbitrate: bit VIA2_PRB  ; is changed to sta VIA2_PRB after init
putnumscts: bit NUMSECTORS; is changed to stx NUMSECTORS after init
            rts

sendblock:  ldy #$00
            jsr gcrencode; block index

            ldx #$ff
            ldy #$04; here, the watchdog timer is polled manually because
                    ; an extra-long time-out period is needed since the c64 may
                    ; still be busy decompressing a large chunk of data;
                    ; this is the round counter
            stx VIA2_T1C_H; set watchdog time-out, this also clears the possibly
                          ; pending timer 1 irq flag
            lda #DATA_OUT
            sta VIA1_PRB; block ready signal
            ; a watchdog is used because the c64 might be reset while sending
            ; over the block, leaving the drive waiting for handshake pulses
waitready:  bit VIA2_IFR; see if the watchdog barked
            bpl :+
            dey        ; if yes, decrease the round counter
            beq timeout; and see if we've already timed out
            stx VIA2_T1C_H; set watchdog time-out and clear irq flag
:           bit VIA1_PRB
            bpl waitready
            stx VIA2_T1C_H; set watchdog time-out and clear possibly set irq flag

timeout:    cli; enable watchdog
            ldy #$00
sendloop:   ldx LONIBBLES+$00,y; 4
            lda SENDGCRTABLE,x ; 4
            ldx HINIBBLES+$00,y; 4

:           bit VIA1_PRB
            bmi :-
            sta VIA1_PRB

            asl                ; 2
            ora #ATNA_OUT      ; 2
            sec                ; 2

:           bit VIA1_PRB
            bpl :-
            sta VIA1_PRB

            ror VIA2_T1C_H     ; 6 ; set watchdog time-out
            lda SENDGCRTABLE,x ; 4

:           bit VIA1_PRB
            bmi :-
            sta VIA1_PRB

            asl                ; 2
            ora #ATNA_OUT      ; 2
dsendcmp:   cpy #$00           ; 2
            iny                ; 2

:           bit VIA1_PRB
            bpl :-
            sta VIA1_PRB
            bcc sendloop

    .assert .hibyte(* + 1) = .hibyte(sendloop), error, "***** Page boundary crossing in byte send loop, fatal cycle loss. *****"

:           bit VIA1_PRB
            bmi :-
            ldy #CLK_OUT
            dec SECTORCOUNT
            bne drivebusy41+$02; pull DATA_OUT high when changing tracks
drvwait0:   ldy #CLK_OUT | DATA_OUT; flag track change
            SKIPWORD

            ; following code is transferred using KERNAL routines, then it is
            ; run and gets the rest of the code

drivebusy41:
            ldy #CLK_OUT
            sty VIA1_PRB
            sei; disable watchdog
            rts

dgetbyte:   lda #%10000000; CLK OUT lo: drive is ready
            sta VIA1_PRB
:           ldy #DATA_OUT | DATA_IN
:           cpy VIA1_PRB
            bcs :-
            ldy VIA1_PRB
            cpy #CLK_IN | DATA_IN
            ror
:           cpy VIA1_PRB
            beq :-
            ldy VIA1_PRB
            cpy #DATA_IN
            ror
            bcc :---
            rts

    .assert * <= $05ff, error, "***** 1541 drive code too large, please try changing options in config.inc. *****"

            ; entry point

dinstall:
.if UNINSTALL_RUNS_DINSTALL
            sei
            lda ROMOS_HEADER_ID0
            sta JOBCODE0300
            lda ROMOS_HEADER_ID1
            sta JOBCODE0400
            lda ROMOS_HEADER_TRACK
            sta JOBCODE0500
            lda ROMOS_HEADER_SECTOR
            sta JOBCODE0600
.endif
            jsr drivebusy41; does sei
            lda #VIA_ATN_IN_INPUT | VIA_PIO7_INPUT | VIA_ATNA_OUT_OUTPUT | VIA_CLK_OUT_OUTPUT | VIA_CLK_IN_INPUT | VIA_DATA_OUT_OUTPUT | VIA_DATA_IN_INPUT | VIA_DEVICE_NUMBER_OUTPUT
            sta VIA1_DDRB

:           lda VIA1_PRB; wait for DATA IN = high
            lsr
instalwait: bcc :-
            ldx #.lobyte(stackend-$03)
            txs
.ifndef STANDALONE
            ldx #.lobyte(driveprg41-$01)
dgetrout:   inx
            bne :+
            inc dgetputhi
:           jsr dgetbyte
dgetputhi = *+$02
            sta a:.hibyte(driveprg41-$01) << 8,x
            cpx #.lobyte(drivebusy41-$01)
            bne dgetrout
            dec driveprg41
            bne dgetrout
.endif; STANDALONE
.if UNINSTALL_RUNS_DINSTALL
            lda #IRQ_CLEAR_FLAGS | $7f
            sta VIA1_IER; no irqs from via 1
            sta VIA2_IER; no irqs from via 2
.endif
            rts; jumps to dcodinit

drvprgend:
            .reloc
