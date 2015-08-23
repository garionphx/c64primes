
.include "cpu.inc"
.include "via.inc"

DISKCHANGEBUFFER    = $00
GCRBUFFER           = $01; $07 bytes
CURSTPSL            = $02
TRACKINC            = $03
GCRBUFFER0          = $08
GCRBUFFER1          = $09
GCRBUFFER2          = $0a
LOADEDSECTOR        = $0b
BLOCKINDEX          = $0c
NUMSECTORS          = $0d
BLOCKINDEXBASE      = $0e
NEXTSECTOR          = $0f
SECTORTOFETCH       = $10
SECTORCOUNT         = $11
CURTRACK            = $12
REQUESTEDSECTOR     = $13
CHECKSUM            = $14

ID0                 = $16; = ROMOS_HEADER_ID0
ID1                 = $17; = ROMOS_HEADER_ID1
CHAR0BUF            = $18; = ROMOS_HEADER_TRACK
CHAR1BUF            = $19; = ROMOS_HEADER_SECTOR

TRACKLINKTAB        = $1a; $15 bytes

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (!LOAD_ONCE)
DIRCYCLECOUNT       = $15

HASHVALUE0LO        = $1a
HASHVALUE0HI        = $1b
HASHVALUE1LO        = $1c
HASHVALUE1HI        = $1d
FILENAME            = $1e; $11 bytes

CYCLESTARTENDSECTOR = $2f
CURRDIRBLOCKSECTOR  = $30
DIRBLOCKPOS         = $31
NEXTDIRBLOCKTRACK   = $32
NEXTDIRBLOCKSECTOR  = $33
NUMFILES            = $34
DDIRBUFF            = $35
.endif


BLOCKBUFFER         = $0700
SECTORLINKTAB       = $0780
TEMPTRACKLINKTAB    = $07c0

ROMOS_HEADER_ID0    = $16
ROMOS_HEADER_ID1    = $17
ROMOS_HEADER_TRACK  = $18
ROMOS_HEADER_SECTOR = $19

DECGCRTAB10ZZZ432LO = $9f0d
DECGCRTAB3210ZZZ4LO = $9f0f
DECGCRTAB0ZZZ4321HI = $9f1d
DECGCRTAB210ZZZ43HI = $9f2a
DECGCRTAB43210XXXHI = $a00d
DECGCRTABXX43210XHI = $a10d
DECGCRTABX43210XXLO = $a20d
DECGCRTABXXX43210LO = $a30d

GCRDECODEHI         = $f8a0
GCRDECODELO         = $f8c0

LINKTRACK           = BLOCKBUFFER + $00
LINKSECTOR          = BLOCKBUFFER + $01


BINARY_NIBBLE_MASK  = %00001111
GCR_NIBBLE_MASK     = %00011111

NUMMAXSECTORS       = 21
NUMTRACKS_A         = 35
NUMTRACKS_B         = 41
MAXTRACK71          = NUMTRACKS_A + NUMTRACKS_B

ANYSECTOR           = $40
UNPROCESSEDSECTOR   = $ff


.if !UNINSTALL_RUNS_DINSTALL
    .org $0068
.else
    .org $005e
.endif

.if UNINSTALL_RUNS_DINSTALL
    .export driveprg71  : absolute
    .export drivebusy71 : absolute
.endif; UNINSTALL_RUNS_DINSTALL

.export MAXTRACK71
.export c1570fix0
.export c1570fix1
.export c1570fix2
.export c1570fix3
.export c1570fix4


driveprg71: .byte .hibyte(drivebusy71-*+$0100-$01); init transfer count hi-byte
dcodinit:   jsr drivebusy71
            jsr two_mhz

            lda #T1_IRQ_ON_LOAD | PA_LATCHING_ENABLE; watchdog irq: count phi2 pulses, one-shot;
            sta VIA2_ACR                            ; port a latching should not be needed here,
                                                    ; but is enabled just to be sure
            lda #READ_MODE; BYTE_SYNC is disabled because this is not done via the v-flag here
            sta VIA2_PCR  ; but rather using bit 7 of VIA1_PRA

            ; before loading the first file, the current track number is
            ; retrieved by reading any block header on the disk -
            ; however, if the loader is uninstalled before loading anything,
            ; it needs the more or less correct current track number to
            ; seek to track 18
            lda ROMOS_HEADER_TRACK
            sta CURTRACK

            ; watchdog initialization
            lda #IRQ_CLEAR_FLAGS | $7f
            sta VIA1_IER; no irqs from via 1
            sta VIA2_IER; no irqs from via 2
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
            sta VIA2_IER; timer 1 irqs from via 2

.if UNINSTALL_RUNS_DINSTALL
            lda #.hibyte(driveprg71-$01)
            sta dgetputhi
            lda #OPC_BIT_ZP
            sta instalwait
.endif; UNINSTALL_RUNS_DINSTALL

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
            sta VIA2_PRB  ; cycle through the 4 bit-rates
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
            sta headrchk0
            lda #OPC_BNE
            sta headerchk
            lda #OPC_STA_ABS
            sta putbitrate
            lda #OPC_STX_ZP
            sta putnumscts
            jmp beginload

SENDNIBBLETAB:
            BIT0DEST = 3
            BIT1DEST = 1
            BIT2DEST = 2
            BIT3DEST = 0

            .repeat $10, I
                .byte (((~I >> 0) & 1) << BIT0DEST) | (((~I >> 1) & 1) << BIT1DEST) | (((~I >> 2) & 1) << BIT2DEST) | (((~I >> 3) & 1) << BIT3DEST)
            .endrep

DISKCHANGEFLAG:
            .word newdisk

          ; * = $0100

    .assert * = $0100, error, "***** Stack not located at $0100. *****"

            ; stack frame
.if LOAD_ONCE
            .res 3; padding, best used for bigger stack
.endif
            .res 4; padding, best used for bigger stack
            .word $00, $00, $00, $00, $00, dcodinit-$01
stackend:
            ; common code for all configurations

waitsyncr:  lda #$ff
            sta VIA2_T1C_H
waitsync:   bit VIA2_T1C_H
            bpl wsynctmout; will return $00 in the accu
            bit VIA2_PRB
            bmi waitsync
            bit VIA2_PRA
:           bit VIA1_PRA
            bmi :-
            ldx #$00
            lda VIA2_PRA; is never $00 but usually $52 (header) or $55 (data)
            rts

gcrtimeout: jsr checkchg; check light sensor for disk removal
            beq :+
            lda #.lobyte(newdisk)
            sta DISKCHANGEFLAG; set the fetch dir flag when disks have been changed
:           sec

            ; fall through

wsynctmout: lda #%00000000
            rts

            ; get the next block passing, check against stored id
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

            lda #OPC_JMP_ABS; full gcr fetch and checksumming
wait4sct:   sta scanswitch

            lda #$ff
            sta VIA2_T1C_H; reset the watchdog timer

            ; returns with carry set on time-out
readblock:  jsr waitsync
            beq gcrtimeout; returns with carry set on time-out
            cmp #%01010010; check if the sync is followed by a sector header
            bne readblock; if not, wait for next sync mark

            ; read the sector header
            ldx #$06
getheader:  bit VIA1_PRA
            bmi getheader
            lda VIA2_PRA
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

            bit REQUESTEDSECTOR
            bmi checkprocd; branch if UNPROCESSEDSECTOR
            bvs waitdatahd; branch if ANYSECTOR

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
waitdatahd: jsr waitsyncr; reset the time-out timer here
            cmp #%01010101; check if the sync is followed by a data block
            bne readblock; if not, wait for next sector

            ldy #$00
            sty CHECKSUM
:           bit VIA1_PRA
            bmi :-
        	ldx VIA2_PRA   ; 11222223
	        cpx #%11000000
	        bcc readblock

loaddata:   lda DECGCRTABXX43210XHI,x; x = [$00..$ff], %2222....
               ; 54 cycles

:           bit VIA1_PRA
            bmi :-
            sta GCRBUFFER0
            txa            ; 11222223
            lsr
            lda VIA2_PRA   ; 33334444
            sta GCRBUFFER1
            and #%11110000 ; 3333....
            adc #%00000000 ; 3333...3
            tax
            lda GCRBUFFER0 ; %2222....            
            ora DECGCRTAB3210ZZZ4LO,x; x = [($00..$ff) & $f1], %22223333
            sta BLOCKBUFFER+$00,y
            eor CHECKSUM
            asl GCRBUFFER1 ; 3334444.
               ; 47 cycles

:           bit VIA1_PRA
            bmi :-
            sta CHECKSUM
            lda VIA2_PRA   ; 45555566
            sta GCRBUFFER0
            asl            ; 5555566.
            lda #%00011110
            and GCRBUFFER1 ; ...4444.
            ror            ; 4...4444
            tax            
            lda DECGCRTAB0ZZZ4321HI,x; x = [($00..$ff) & $8f], %4444....
            ldx GCRBUFFER0 ; 45555566
            ora DECGCRTABX43210XXLO,x; x = [$00..$ff], %44445555
            sta BLOCKBUFFER+$01,y
               ; 48 cycles

:           bit VIA1_PRA
            bmi :-
            eor CHECKSUM
            sta CHECKSUM
            txa            ; 45555566
            and #%00000011 ; ......66
            sta GCRBUFFER0
            lda VIA2_PRA   ; 66677777
            sta GCRBUFFER1
            and #%11100000 ; 666.....
            ora GCRBUFFER0 ; 666...66
            tax
            lda DECGCRTAB210ZZZ43HI,x; x = [($00..$ff) & $e3], %6666....
            ldx GCRBUFFER1 ; 66677777
            ora DECGCRTABXXX43210LO,x; x = [$00..$ff], %66667777
               ; 46 cycles

:           bit VIA1_PRA
            bmi :-
            sta BLOCKBUFFER+$02,y
            eor CHECKSUM
            sta CHECKSUM
            ldx VIA2_PRA   ; 00000111
            lda DECGCRTAB43210XXXHI,x; x = [$00..$ff], %0000....
            sta GCRBUFFER1
            txa
            and #%00000111 ; .....111
            sta GCRBUFFER2
            iny
            iny
            iny
               ; 42 cycles

:           bit VIA1_PRA
            bmi :-
            lda VIA2_PRA   ; 11222223
            sta GCRBUFFER0
            and #%11000000 ; 11......
            ora GCRBUFFER2 ; 11...111
            tax
            lda DECGCRTAB10ZZZ432LO,x; x = [($00..$ff) & $87]; %....1111
            ora GCRBUFFER1 ; %00001111
            sta BLOCKBUFFER+$00,y
            eor CHECKSUM
            sta CHECKSUM
            ldx GCRBUFFER0 ; 11222223
            iny
            beq :+
scanswitch: jmp loaddata
               ; 49 cycles
            
:           bne :++; don't check sum if only the first few bytes have been
                   ; decoded for scanning
            lda DECGCRTABXX43210XHI,x; x = [$00..$ff], %2222....
            sta GCRBUFFER0
            txa            ; 11222223
            lsr            ; .1122222
:           bit VIA1_PRA
            bmi :-
            lda VIA2_PRA   ; 33334444
            and #%11110000 ; 3333....
            adc #%00000000 ; 3333...3
            tax
            lda GCRBUFFER0 ; %2222....
            ora DECGCRTAB3210ZZZ4LO,x; x = [($00..$ff) & $f1], %22223333
            eor CHECKSUM
            beq :+; check whether data checksum is ok
jpreadbk:   jmp readblock; if not, wait for next sector

            ; checksum sector header
            ; this is done only now because there is no time for that between
            ; the sector header and data block
:           lda GCRBUFFER+$06
            lsr
            and #GCR_NIBBLE_MASK
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
headrchk0:  sta CURTRACK; is changed to eor CURTRACK after init
            jmp headerchk

    .assert * = $02a9, error, "***** 1571 watchdog IRQ vector not located at $02a9. *****"
            .word watchdgirq

headerchk:  .byte OPC_BIT_ZP, .lobyte(jpreadbk-*-$01); is changed to bne jpreadbk
                                        ; after init, wait for next sector if
                                        ; sector header checksum was not ok

            lda GCRBUFFER+$05; ID1
            ldx #$00; set z-flag which won't be altered by the store opcodes
storputid0: cpy ID0; cpy ID0/sty ID0
            bne :+
storputid1: cmp ID1; cmp ID1/sta ID1
:           clc
dsctcmps:   .byte OPC_RTS, .lobyte(jpreadbk-*-$01); is changed to bne jpreadbk
                                            ; branch if some error occured while
                                            ; checking the sector header



            ; sector link sanity check
            lda LINKSECTOR; return the loaded block's sector link sector number
            ldy LINKTRACK ; return the loaded block's sector link track number
            beq :+
c1570fix0 = *+1
            cpy #MAXTRACK71+1; check whether track link is within the valid range
            bcs jpreadbk  ; if not, wait for next sector
            cmp NUMSECTORS; check whether sector link is within the valid range
            bcs jpreadbk  ; if not, wait for next sector

:           ldx LOADEDSECTOR; return the loaded block's sector number
           ;clc             ; operation successful
            rts

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

ddliteon:   lda #BUSY_LED
            ora VIA2_PRB
            bne store_via2

trackseek:
c1570fix2 = *+$01
            cmp #MAXTRACK71+1
            bcs setbitrate; don't do anything if invalid track

            ; do the track jump

            sec
            tax; buffer new track number
            lda CURTRACK
            sbc #NUMTRACKS_A
            beq :+
            bcc :+
            sta CURTRACK; the current track is on the 2nd side,
                        ; temporarily store the 2nd side physical track number
:           sec
            txa
            sbc #NUMTRACKS_A
            beq :+
            bcs :++
:           clc
            txa; the destination track is on the 1st side
:           pha
            lda VIA1_PRA
            ora #SIDE_B  ; no idea why this is needed
c1570fix3:  sta VIA1_PRA ; but it won't work without on 1571
            and #~SIDE_SELECT
            bcc :+
            ora #SIDE_B
:
c1570fix4:  sta VIA1_PRA
            pla
            sec
            sbc CURTRACK
            stx CURTRACK; store new track number
            beq setbitrate

            ldy #$00
            sty CURSTPSL
            bcs :+
            eor #~$00; invert track difference
            adc #$01
            iny
:           sty TRACKINC
            asl
            tay
            jsr one_mhz
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
setbitrate: lda CURTRACK
            tay
            sec
            sbc #NUMTRACKS_A
            beq :+
            bcc :+
            tay
:           lda VIA2_PRB
            ora #SYNC_MARK | BITRATE_MASK
            ldx #$15
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
putbitrate: bit VIA2_PRB  ; is set to sta VIA2_PRB after init
putnumscts: bit NUMSECTORS; is changed to stx NUMSECTORS after init

            ; fall through

two_mhz:    lda #TWO_MHZ | BYTE_READY
            ora VIA1_PRA
            sta VIA1_PRA
            rts

            ; for normal busy led fading speed and correct head
            ; stepping speed
one_mhz:    lda #~TWO_MHZ
            and VIA1_PRA
            sta VIA1_PRA
            rts

            ror
decodesub:  lsr
            lsr
            lsr
            tax
            lda GCRDECODEHI,y
            ora GCRDECODELO,x
            rts

checkchg:   lda VIA2_PRB
            and #WRITE_PROTECT
            cmp DISKCHANGEBUFFER
            sta DISKCHANGEBUFFER
            rts

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
            jmp dinstall

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

drividle:   jsr one_mhz
idleloop:   jsr lightsub; fade off the busy led
            jsr gcrtimeout; check light sensor for disk removal
            lda VIA1_PRB
            cmp #CLK_OUT | CLK_IN | DATA_IN | ATN_IN
            beq idleloop; wait until there is something to do

            cmp #CLK_OUT | CLK_IN | DATA_IN
            bne duninstall; check for reset or uninstallation

            ; load a file

            txa
            beq beginload; check whether the busy led has been completely faded off
            jsr ddliteon; if not, turn it on

beginload:

.if !LOAD_ONCE ; not with LOAD_ONCE because then, there is no danger of getting stuck
               ; because there is no serial transfer to retrieve the file id
            lda #$ff
            sta VIA2_T1C_H; set watchdog time-out, this also clears the possibly
                          ; pending timer 1 irq flag
            cli; enable watchdog, the c64 might be reset while sending over
               ; a byte, leaving the drive waiting for handshake pulses

            ; get starting track and sector of the file to load

    .if ::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME

            ldx #-$01
            stx DIRCYCLECOUNT
getfilenam: inx
            jsr dgetbyte; get filename
            beq :+
            sta FILENAME,x
            cpx #FILENAME_MAXLENGTH
            bne getfilenam
:           jsr drvwait0; disables watchdog
            jsr gethashval
            sta CHAR1BUF
            stx CHAR0BUF

    .elseif ::FILESYSTEM = FILESYSTEMS::TRACK_SECTOR

            jsr dgetbyte; get starting track
            sta CHAR0BUF
            jsr dgetbyte; get starting sector
            jsr drvwait0; disables watchdog
            sta CHAR1BUF

    .endif
.endif

.if (::FILESYSTEM = FILESYSTEMS::TRACK_SECTOR) || LOAD_ONCE
            ; check for illegal track or sector
            ldy CHAR0BUF
            beq toillegal+$00
c1570fix1 = *+$01
            cpy #MAXTRACK71+1
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

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (LOAD_ONCE = 0)

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
            bne :-; store disk id if disk changed
            lda NEXTDIRBLOCKTRACK
            bne :+
            lda NUMFILES    ; leave here if there are less files in the
            cmp #DIRBUFFSIZE; directory than the dir buffer can hold
            bcc samedisk
            lda #DIRSECTOR
            ldy #DIRTRACK
           ;sec
            bcs dirwrap; jmp

:           ldy NEXTDIRBLOCKSECTOR
            jsr getblkchid; compare id, sector link sanity check
            bcs dnxtdirs
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

            lda .lobyte(DIRTRACKS),x
            beq :+; don't increase file counter with non-files
                  ; (marked by start track 0)

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
:           tya
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

            stx CHAR0BUF; store index of file to set track for the next file
                        ; after loading

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
            ldy .lobyte(DIRSECTORS),x
            lda .lobyte(DIRTRACKS),x

            ; check for illegal track or sector
            beq toillegal+$00
c1570fix1 = *+$01
            cmp #MAXTRACK71+1
            bcs toillegal+$01
            cpy NUMSECTORS
            bcc :+
toillegal:  sec
            jmp illegalts
:

.else ; ::FILESYSTEM = FILESYSTEMS::TRACK_SECTOR || !LOAD_ONCE

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

scantrloop: lda #OPC_LDA_ABS; only fetch the first few bytes to track the links
            ; this is a weak point since there is no data checksumming here
            ldy #ANYSECTOR
            sty REQUESTEDSECTOR
            jsr wait4sct
            bcs scantrloop; branch until fetch successful

            ; x contains the loaded block's sector number here
            sta SECTORLINKTAB,x
            lda LINKTRACK
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
            sta TRACKLINKTAB,x; store sector index
            lda SECTORLINKTAB,x; get link sector
            tax
            iny; increase sector index
            pla
            cmp CURTRACK; check whether link track is the current track
            beq numblklp; branch until all the file's blocks on the current
                        ; track have been ordered

            ; read and transfer all the blocks on the current track that belong
            ; to the file, the blocks are read in quasi-random order
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
:
            sty dsendcmp+$01
            dey
            dey
            sty BLOCKBUFFER+$01; block length
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

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (LOAD_ONCE = 0)

            ldx CHAR0BUF
            inx
            cpx NUMFILES
            lda .lobyte(DIRTRACKS),x
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
:
            jmp duninstall
.else
:           cli; enable watchdog
:           bit VIA1_PRB
            bpl :-; wait until the computer has acknowledged the file transfer
            sei; disable watchdog
            jmp drividle
.endif

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (!LOAD_ONCE)

fnamehash:  lda BLOCKBUFFER+$00,y; get file's start track
            sta .lobyte(DIRTRACKS),x
            lda BLOCKBUFFER+$01,y; get file's start sector
            sta .lobyte(DIRSECTORS),x
            ldx #$00
:           lda BLOCKBUFFER+$02,y
            iny
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

sendblock:  sta BLOCKBUFFER+$00; block index
            ldx #$ff
            ldy #$08; here, the watchdog timer is polled manually because
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
            stx VIA2_T1C_H; set watchdog time-out
timeout:    cli; enable watchdog

            ldy #$00
sendloop:   lda #$ff           ; 2
            sta VIA2_T1C_H     ; 4 ; set watchdog time-out
            lda BLOCKBUFFER,y  ; 4
            and #BINARY_NIBBLE_MASK; 2
            tax                ; 2
            lda SENDNIBBLETAB,x; 4

:           bit VIA1_PRB
            bmi :-
            sta VIA1_PRB

            asl                ; 2
            ora #ATNA_OUT      ; 2

:           bit VIA1_PRB
            bpl :-
            sta VIA1_PRB

            lda BLOCKBUFFER,y  ; 4
            lsr                ; 2
            lsr                ; 2
            lsr                ; 2
            lsr                ; 2
            tax                ; 2
            lda SENDNIBBLETAB,x; 4

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

:           bit VIA1_PRB
            bmi :-

            dec SECTORCOUNT
            bne drivebusy71; pull DATA_OUT high when changing tracks
drvwait0:   ldy #CLK_OUT | DATA_OUT; flag track change
            sty VIA1_PRB; this is needed here to set the register in time
            SKIPWORD

            ; following code is transferred using KERNAL routines, then it is
            ; run and gets the rest of the code

drivebusy71:
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

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) & (!LOAD_ONCE) & (!UNINSTALL_RUNS_DINSTALL)
    .if (SENDNIBBLETAB - DDIRBUFF) < ($0800 - *)
        DIRBUFFSIZE  = (SENDNIBBLETAB - DDIRBUFF) / 2
    .else
        DIRBUFFSIZE  = ($0800 - *) / 2
    .endif
    DIRTRACKS        = DDIRBUFF
    DIRSECTORS       = DIRTRACKS + DIRBUFFSIZE
    FILENAMEHASHVAL0 = *
    FILENAMEHASHVAL1 = FILENAMEHASHVAL0 + DIRBUFFSIZE
    DIRBUFFEND       = FILENAMEHASHVAL1 + DIRBUFFSIZE

    .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"
.endif

.if !UNINSTALL_RUNS_DINSTALL
    .assert * <= $0700, error, "***** 1571 drive code too large, please try changing options in config.inc. *****"
.endif
            ; entry point

dinstall:   jsr drivebusy71
            lda #VIA_ATN_IN_INPUT | VIA_PIO7_INPUT | VIA_ATNA_OUT_OUTPUT | VIA_CLK_OUT_OUTPUT | VIA_CLK_IN_INPUT | VIA_DATA_OUT_OUTPUT | VIA_DATA_IN_INPUT | VIA_DEVICE_NUMBER_OUTPUT
            sta VIA1_DDRB

:           lda VIA1_PRB; wait for DATA IN = high
            lsr
instalwait: bcc :-
            ldx #.lobyte(stackend-$03)
            txs
            ldx #.lobyte(driveprg71-$01)
dgetrout:   inx
            bne :+
            inc dgetputhi
:           jsr dgetbyte
dgetputhi = *+$02
            sta a:.hibyte(driveprg71-$01) << 8,x
            cpx #.lobyte(drivebusy71-$01)
            bne dgetrout
            dec driveprg71
            bne dgetrout
            rts; jumps to dcodinit

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) & (LOAD_ONCE = 0) & (UNINSTALL_RUNS_DINSTALL)
    .if (SENDNIBBLETAB - DDIRBUFF) < ($0800 - *)
        DIRBUFFSIZE  = (SENDNIBBLETAB - DDIRBUFF) / 2
    .else
        DIRBUFFSIZE  = ($0800 - *) / 2
    .endif
    DIRTRACKS        = DDIRBUFF
    DIRSECTORS       = DIRTRACKS + DIRBUFFSIZE
    FILENAMEHASHVAL0 = *
    FILENAMEHASHVAL1 = FILENAMEHASHVAL0 + DIRBUFFSIZE
    DIRBUFFEND       = FILENAMEHASHVAL1 + DIRBUFFSIZE

    .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"
.endif

.if UNINSTALL_RUNS_DINSTALL
    .assert * <= $0700, error, "***** 1571 drive code too large, try changing options in config.inc. *****"
.endif

drvprgend:
            .reloc
