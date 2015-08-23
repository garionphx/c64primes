
.include "cia.inc"

BUFFER               = $00
SYS_SP               = $01
CHAR0BUF             = $0b
CHAR1BUF             = $0c
HASHVALUE0LO         = $0d
HASHVALUE0HI         = $0e
HASHVALUE1LO         = $0f
HASHVALUE1HI         = $10
NUMFILES             = $11
CURRDIRBLOCKSECTOR   = $12
CYCLESTARTENDSECTOR  = $13
DIRCYCLECOUNT        = $14
NEXTDIRBLOCKTRACK    = $15
NEXTDIRBLOCKSECTOR   = $16
BLOCKINDEX           = $34
SYSIRQVECTOR_LO      = $5e
SYSIRQVECTOR_HI      = $5f
.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) & (!LOAD_ONCE)
FILEINDEX            = $63
.endif

JOBCODESTABLE        = $02
JOBTRKSCTTABLE       = $0b
OPEN_FILE_TRACK      = $4c
LED_FLAG             = $79
IRQVECTOR_LO         = $0192
IRQVECTOR_HI         = $0193
HDRS2                = $01bc
DIRTRACK81           = $022b
OPEN_FILE_SECTOR     = $028b
DIRSECTOR81          = $00  ; literal

RESET_TIMERB         = $cb9f
STROBE_CONTROLLER    = $ff54

READ_DV              = $80
SEEK_DV              = $8c

OK_DV                = $00

BLOCKBUFFER          = $0900
SENDTABLELO          = $0a00
SENDTABLEHI          = $0b00

LINKTRACK            = BLOCKBUFFER+$00
LINKSECTOR           = BLOCKBUFFER+$01

BINARY_NIBBLE_MASK   = %00001111

MAXTRACK81           = 80
MAXSECTOR81          = 39

WATCHDOG_PERIOD      = $08; 8 * 65536 cycles at 2 MHz = 262 ms


.org $0300


.if UNINSTALL_RUNS_DINSTALL
    .export driveprg81  : absolute
    .export drivebusy81 : absolute
.endif; UNINSTALL_RUNS_DINSTALL

driveprg81: .byte .hibyte(drivebusy81 - * + $0100 - $01); init transfer count hi-byte

SENDNIBBLETAB:
            BIT0DEST = 3
            BIT1DEST = 1
            BIT2DEST = 2
            BIT3DEST = 0

            .repeat $10, I
                .byte (((~I >> 0) & 1) << BIT0DEST) | (((~I >> 1) & 1) << BIT1DEST) | (((~I >> 2) & 1) << BIT2DEST) | (((~I >> 3) & 1) << BIT3DEST)
            .endrep

filename:

dcodinit:   jsr drivebusy81

            tsx
            stx SYS_SP

            ldx #$00
:           txa
            and #BINARY_NIBBLE_MASK
            tay
            lda SENDNIBBLETAB,y
            sta SENDTABLELO,x
            txa
            lsr
            lsr
            lsr
            lsr
            tay
            lda SENDNIBBLETAB,y
            sta SENDTABLEHI,x
            inx
            bne :-

            ; watchdog initialization
            lda IRQVECTOR_LO
            sta SYSIRQVECTOR_LO
            lda IRQVECTOR_HI
            sta SYSIRQVECTOR_HI
            lda #$ff
            sta CIA_TA_LO
            sta CIA_TA_HI
            lda #COUNT_PHI2 | FORCE_LOAD | CONTINUOUS | TIMER_START
            sta CIA_CRA
            jsr initwatchd
            lda #CIA_CLR_INTF | EVERY_IRQ
            sta CIA_ICR
            lda #CIA_SET_INTF | TIMERB_IRQ
            sta CIA_ICR

.if UNINSTALL_RUNS_DINSTALL
            lda #.hibyte(driveprg81-$01)
            sta dgetputhi
            lda #OPC_BIT_ZP
            sta instalwait
.endif; UNINSTALL_RUNS_DINSTALL

            ldx #$00

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) & (!LOAD_ONCE)
            stx NUMFILES
.endif
            jmp drividle

.if UNINSTALL_RUNS_DINSTALL

watchdgirq: ldx #$ff
            ; fade off the busy led and reset the drive
:           jsr lightsub
            txa
            bne :-
            ; the 1581 moves the head to track 1 after reset,
            ; so it's not moved to the directory track here
            jmp (RESET_VECTOR)
duninstall:
:           jsr lightsub
            txa
            bne :-
            jsr initcontrl; restore the irq vector
            jmp dinstall

.else

watchdgirq: ldx #$ff

duninstall: txa
            pha
            lda DIRTRACK81
            jsr trackseek
            pla
            ; fade off the busy led (if lit) and reset the drive
            beq :++
            ldx #$ff
:           jsr lightsub
            txa
            bne :-
:           ldx SYS_SP
            txs
            jmp initcontrl
.endif

drividle:   jsr lightsub; fade off the busy led
            lda CIA_PRB
            and #ATN_IN | ATNA_OUT | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            cmp #ATN_IN |            CLK_OUT | CLK_IN |            DATA_IN
            beq drividle; wait until there is something to do

            cmp #                    CLK_OUT | CLK_IN |            DATA_IN
            bne duninstall; check for reset or uninstallation

            ; load a file

            txa
            beq beginload; check whether the busy led has been completely faded off
            jsr ddliteon ; if not, turn it on
beginload:
.if LOAD_ONCE
            lda OPEN_FILE_TRACK
            sta CHAR0BUF
            lda OPEN_FILE_SECTOR
            sta CHAR1BUF
.else
            jsr enablewdog; enable watchdog, the c64 might be reset while sending over a
                          ; byte, leaving the drive waiting for handshake pulses

            ; get starting track and sector of the file to load

    .if ::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME

            ldx #-$01
getfilenam: inx
            jsr dgetbyte; get filename
            beq :+
            sta filename,x
            cpx #FILENAME_MAXLENGTH
            bne getfilenam
:           jsr drvwait0; disables watchdog
            jsr gethashval
            sta CHAR1BUF
            stx CHAR0BUF

            lda NUMFILES
            beq newdisk
            lda CIA_PRA
            bmi samedisk

newdisk:    lda DIRTRACK81
            ldx #DIRSECTOR81
            jsr getblock
            bcs newdisk

           ;clc
            sta CYCLESTARTENDSECTOR

dirwrap:    sta NEXTDIRBLOCKSECTOR
            sty NEXTDIRBLOCKTRACK
            bcs dnxtdirs
            ; directory cycling: fill the dir buffer
filldirbuf: lda #$00
            sta NUMFILES
dnxtdirs:   lda NEXTDIRBLOCKTRACK
            bne :+
            lda NUMFILES    ; leave here if there are less files in the
            cmp #DIRBUFFSIZE; directory than the dir buffer can hold
            bcc samedisk
            lda #DIRSECTOR81
            ldy DIRTRACK81
           ;sec
            bcs dirwrap; jmp

:           ldx NEXTDIRBLOCKSECTOR
            jsr getblock
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

            lda DIRTRACKS,x
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
samedisk:   ldy #-$01
findfile:   iny
            cpy NUMFILES
            bne :+; check all dir entries in the buffer

            ; the dir buffer does not contain the file,
            ; so cycle through the directory to find it

           ;lda DIRTRACK81       ; not needed since NEXTDIRBLOCKTRACK
           ;sta NEXTDIRBLOCKTRACK; is always left holding DIRTRACK

            ldx DIRCYCLECOUNT    ; check if a cycle
            dex                  ; is complete
            bmi filldirbuf       ; the counter will increase on cycle start
            jmp filenotfnd

:           lda CHAR0BUF
            cmp FILENAMEHASHVAL0,y
            bne findfile
            lda CHAR1BUF
            cmp FILENAMEHASHVAL1,y
            bne findfile

            sty FILEINDEX; store index of file to jump to the track of the file
                         ; following this one in the dir, after loading

            ; store track and sector of the dir block the file was found in,
            ; they are used to start the dir check cycle if the next file is not found
            ; in the dir buffer;
            ; they are also checked on the subsequent load to determine if the dir check
            ; cycle is complete and the file be said to be not found
            lda CURRDIRBLOCKSECTOR
            sta CYCLESTARTENDSECTOR
            sta NEXTDIRBLOCKSECTOR
            lda DIRTRACK81
            sta NEXTDIRBLOCKTRACK

            jsr ddliteon
            lda DIRTRACKS,y
            sta CHAR0BUF
            lda DIRSECTORS,y
            sta CHAR1BUF

    .elseif ::FILESYSTEM = FILESYSTEMS::TRACK_SECTOR

            jsr dgetbyte; get starting track
            sta CHAR0BUF
            jsr dgetbyte; get starting sector
            jsr drvwait0; disables watchdog
            sta CHAR1BUF

    .endif
.endif; !LOAD_ONCE

            ; check for illegal track or sector
            lda CHAR0BUF
            beq toillegal+$00
            cmp #MAXTRACK81+1
            bcs toillegal+$01
            ldx CHAR1BUF
            cpx #MAXSECTOR81+1
            bcc :+
toillegal:  sec
            jmp illegalts

:           jsr ddliteon
            lda CHAR0BUF

            ldy #$00
            sty BLOCKINDEX
loadblock:  sta JOBTRKSCTTABLE + ((BLOCKBUFFER - $0300) / 128) + 0
            stx JOBTRKSCTTABLE + ((BLOCKBUFFER - $0300) / 128) + 1
:           jsr getblockag
            bcs :-

            ldy #$ff
            lda LINKTRACK
            pha
            bne :+
            ldy LINKSECTOR; the file's last block's length
:           lda LINKSECTOR
            pha
            sty dsendcmp+$01
            dey
            dey
            sty BLOCKBUFFER+$01; block length
            lda BLOCKINDEX
            jsr sendblock; send the block over
            inc BLOCKINDEX
            pla; LINKSECTOR
            tax
            pla; LINKTRACK
            bne loadblock

            ; loading is finished

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) && (LOAD_ONCE = 0)

            ldx FILEINDEX
            inx
            cpx NUMFILES
            lda DIRTRACKS,x
            bcc :+
            lda DIRTRACK81
:           jsr trackseek

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
            sta JOBTRKSCTTABLE + ((BLOCKBUFFER - $0300) / 128) + 0; make sure that a track
                                                                  ; change is not flagged
            jsr sendblock; send status

            ldx #$01; turn motor and busy led off
            lda #DRIVE_LED; check if busy led is lit
            and CIA_PRA
            beq :+
            ldx #$ff; fade off the busy led, then turn motor off
.if LOAD_ONCE
:
            jmp duninstall
.else
:           jsr enablewdog
:           bit CIA_PRB
            bpl :-; wait until the computer has acknowledged the file transfer
            sei; disable watchdog
            jmp drividle
.endif

initcontrl: lda CIA_PRA
            and #DRIVE_LED
            beq :+
            lda #$ff
:           sta LED_FLAG
            lda SYSIRQVECTOR_LO
            sta IRQVECTOR_LO
            lda SYSIRQVECTOR_HI
            sta IRQVECTOR_HI
            jmp RESET_TIMERB

trackseek:  tax
            dex
            stx HDRS2 + ((BLOCKBUFFER - $0300) / 128)
            jsr initcontrl
            lda #SEEK_DV
            ldx #(BLOCKBUFFER - $0300) / 256
            jsr STROBE_CONTROLLER; move head to the start track of the next file in the
                                 ; directory

            ; fall through

initwatchd: ; the i-flag is set here
            lda #.lobyte(watchdgirq)
            sta IRQVECTOR_LO
            lda #.hibyte(watchdgirq)
            sta IRQVECTOR_HI
            lda #.lobyte(WATCHDOG_PERIOD)
            sta CIA_TB_LO
            lda #.hibyte(WATCHDOG_PERIOD)
            sta CIA_TB_HI
            rts

enablewdog: lda #COUNT_TA_UNDF | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB
            bit CIA_ICR
            cli
            rts

lightsub:   txa
            tay
            beq ddliteon-$01
:           nop
            bit OPC_BIT_ZP
            adc #$01
            bne :-
            jsr ddliteon
:           nop
            bit OPC_BIT_ZP
            dey
            bne :-
            dex
            bne :+
            ora #MOTOR     ; turn off motor
:           and #~DRIVE_LED; turn off drive led
store_cia:  sta CIA_PRA
            rts

ddliteon:   lda #DRIVE_LED
            ora CIA_PRA
            bne store_cia

getblock:   sta JOBTRKSCTTABLE + ((BLOCKBUFFER - $0300) / 128) + 0
            stx JOBTRKSCTTABLE + ((BLOCKBUFFER - $0300) / 128) + 1            
getblockag: jsr initcontrl
            lda #READ_DV
            ldx #(BLOCKBUFFER - $0300) / 256
            jsr STROBE_CONTROLLER
            jsr initwatchd
            lda JOBCODESTABLE + ((BLOCKBUFFER - $0300) / 256); FD does not return the error status in the accu
            cmp #OK_DV + 1
            lda LINKSECTOR
            ldx JOBTRKSCTTABLE + ((BLOCKBUFFER - $0300) / 128) + 1
            ldy LINKTRACK
            rts

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) & (!LOAD_ONCE)

fnamehash:  lda BLOCKBUFFER+$00,y; get file's start track
            sta DIRTRACKS,x
            lda BLOCKBUFFER+$01,y; get file's start sector
            sta DIRSECTORS,x
            ldx #$00
:           lda BLOCKBUFFER+$02,y
            iny
            cmp #' ' | $80
            beq gethashval
            sta filename,x
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
hashloop:   lda filename - 1,x
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
            jsr enablewdog
            lda #DATA_OUT
            sta CIA_PRB; block ready signal
waitready:  bit CIA_PRB
            bpl waitready
            ldy #$00
sendloop:   lda #COUNT_TA_UNDF | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB      ; 2 + 4; service watchdog

            ldx BLOCKBUFFER,y; 4
            lda SENDTABLELO,x; 4
:           bit CIA_PRB
            bmi :-
            sta CIA_PRB

            asl              ; 2
            and #~ATNA_OUT   ; 2

:           bit CIA_PRB
            bpl :-
            sta CIA_PRB

            ldx BLOCKBUFFER,y; 4
            lda SENDTABLEHI,x; 4
:           bit CIA_PRB
            bmi :-
            sta CIA_PRB

            asl              ; 2
            and #~ATNA_OUT   ; 2

dsendcmp:   cpy #$00         ; 2
            iny              ; 2

:           bit CIA_PRB
            bpl :-
            sta CIA_PRB
            bcc sendloop

:           bit CIA_PRB
            bmi :-
            lda LINKTRACK
            cmp JOBTRKSCTTABLE + ((BLOCKBUFFER - $0300) / 128) + 0
            beq drivebusy81; pull DATA_OUT high when changing tracks
drvwait0:   ldy #CLK_OUT | DATA_OUT; flag track change
            SKIPWORD

            ; following code is transferred using KERNAL routines, then it is
            ; run and gets the rest of the code

drivebusy81:
            ldy #CLK_OUT
            sty CIA_PRB
            sei; disable watchdog
            rts

dgetbyte:   lda #%10000000; CLK OUT lo: drive is ready
            sta BUFFER
            sta CIA_PRB
:           lda #CLK_IN
:           bit CIA_PRB
            beq :-
            lda CIA_PRB
            lsr
            ror BUFFER
            lda #CLK_IN
:           bit CIA_PRB
            bne :-
            lda CIA_PRB
            lsr
            ror BUFFER
            bcc :---
            lda BUFFER
            rts

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) & (!LOAD_ONCE) & (!UNINSTALL_RUNS_DINSTALL)
    DIRBUFFSIZE      = (BLOCKBUFFER - *) / 4
    DIRTRACKS        = *
    DIRSECTORS       = DIRTRACKS + DIRBUFFSIZE
    FILENAMEHASHVAL0 = DIRSECTORS + DIRBUFFSIZE
    FILENAMEHASHVAL1 = FILENAMEHASHVAL0 + DIRBUFFSIZE
    DIRBUFFEND       = FILENAMEHASHVAL1 + DIRBUFFSIZE

    .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"
.endif

.if !UNINSTALL_RUNS_DINSTALL
    .assert * <= BLOCKBUFFER, error, "***** 1571 drive code too large, please try changing options in config.inc. *****"
.endif
            ; entry point

dinstall:   jsr drivebusy81; does sei

:           lda CIA_PRB; wait for DATA IN = high
            lsr
instalwait: bcc :-
            ldx #.lobyte(driveprg81-$01)
dgetrout:   inx
            bne :+
            inc dgetputhi
:           jsr dgetbyte
dgetputhi = *+$02
            sta a:.hibyte(driveprg81-$01) << 8,x
            cpx #.lobyte(drivebusy81-$01)
            bne dgetrout
            dec driveprg81
            bne dgetrout
            jmp dcodinit

.if (::FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME) & (LOAD_ONCE = 0) & (UNINSTALL_RUNS_DINSTALL)
    DIRBUFFSIZE      = (BLOCKBUFFER - *) / 4
    DIRTRACKS        = *
    DIRSECTORS       = DIRTRACKS + DIRBUFFSIZE
    FILENAMEHASHVAL0 = DIRSECTORS + DIRBUFFSIZE
    FILENAMEHASHVAL1 = FILENAMEHASHVAL0 + DIRBUFFSIZE
    DIRBUFFEND       = FILENAMEHASHVAL1 + DIRBUFFSIZE

    .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"
.endif

.if UNINSTALL_RUNS_DINSTALL
    .assert * <= BLOCKBUFFER, error, "***** 1571 drive code too large, please try changing options in config.inc. *****"
.endif

drvprgend:
            .reloc
