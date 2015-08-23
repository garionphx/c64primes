
DYNLINKLDR   = 0
INSTALL_ONLY = 0

BITMAP  = $4000
SPRITES = $6800
COMPBMP = $7000

.define VALIDATE          0; validate correct loading by loading twice and comparing
.define PAUSEBETWEENLOADS 0

.include "../include/loader.inc"
.include "../version.inc"

.include "zp.inc"

.include "cpu.inc"
.include "vic.inc"
.include "cia.inc"

.include "float.inc"
.include "basic.inc"
.include "kernal.inc"

.macpack cbm
.macpack longbranch


.if DYNLINKLDR

    .segment "DATA"
diskio:     .tag diskio::IMPORT
            ; here, the requested functions are defined

            ; some functionality is different from usual applications
            USER_TIMER_FEATS = 1
            .macro USER_TIMER_FEATS_IMPL
                ; this application needs both CIA1 timers for measuring kb/s
                .byte diskio::DOESNT_USE_CIA1_TA_IRQ, diskio::CIA1_TA_IRQ_NOT_USED
                .out "  must not use CIA 1 TA IRQ"
                .byte diskio::DOESNT_USE_CIA1_TB_IRQ, diskio::CIA1_TB_IRQ_NOT_USED
                .out "  must not use CIA 1 TB IRQ"
            .endmacro

            ; use the default dynlink configuration settings to generate the feature list
            ; and also redefine the symbols to point to module space;
            ; some default settings are changed by above .defines
            .include "install-jumptable.inc"
            .include "loader-jumptable.inc"

            ; give the (initially empty) loader segments some size
            .segment "LOADERINSTALL"
            .if (UNINSTALL_RUNS_DINSTALL & DYNLINKLDR & (!LOAD_ONCE))
                ; here, the drivecode is referenced by the upload code,
                ; so it is actually statically linked, and later overwritten by the dynamically-linked install code;
                ; the segment is already large enough for the jumptable-less install code, but also reserve a few bytes
                ; for the jumptable and the dynlink tables which are generated while importing
                .res DYNLINKOVERHEAD
            .else
                .res $1800
            .endif

            .segment "LOADER"
            .res $0800
.endif


; name/t&s parameters

.if FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME

    .define X_SHEBA_UNCOMPRESSED_1541 .lobyte(shebaunc)
    .define Y_SHEBA_UNCOMPRESSED_1541 .hibyte(shebaunc)

    .define X_SHEBA_UNCOMPRESSED_1571 X_SHEBA_UNCOMPRESSED_1541
    .define Y_SHEBA_UNCOMPRESSED_1571 Y_SHEBA_UNCOMPRESSED_1541

    .define X_SHEBA_UNCOMPRESSED_1581 X_SHEBA_UNCOMPRESSED_1541
    .define Y_SHEBA_UNCOMPRESSED_1581 Y_SHEBA_UNCOMPRESSED_1541


    .define X_PRHEI_UNCOMPRESSED_1541 .lobyte(prheiunc)
    .define Y_PRHEI_UNCOMPRESSED_1541 .hibyte(prheiunc)

    .define X_PRHEI_UNCOMPRESSED_1571 X_PRHEI_UNCOMPRESSED_1541
    .define Y_PRHEI_UNCOMPRESSED_1571 Y_PRHEI_UNCOMPRESSED_1541

    .define X_PRHEI_UNCOMPRESSED_1581 X_PRHEI_UNCOMPRESSED_1541
    .define Y_PRHEI_UNCOMPRESSED_1581 Y_PRHEI_UNCOMPRESSED_1541


    .define X_SHEBA_COMPRESSED_1541 .lobyte(shebacompd)
    .define Y_SHEBA_COMPRESSED_1541 .hibyte(shebacompd)

    .define X_SHEBA_COMPRESSED_1571 X_SHEBA_COMPRESSED_1541
    .define Y_SHEBA_COMPRESSED_1571 Y_SHEBA_COMPRESSED_1541

    .define X_SHEBA_COMPRESSED_1581 X_SHEBA_COMPRESSED_1541
    .define Y_SHEBA_COMPRESSED_1581 Y_SHEBA_COMPRESSED_1541


    .define X_PRHEI_COMPRESSED_1541 .lobyte(prheicompd)
    .define Y_PRHEI_COMPRESSED_1541 .hibyte(prheicompd)

    .define X_PRHEI_COMPRESSED_1571 X_PRHEI_COMPRESSED_1541
    .define Y_PRHEI_COMPRESSED_1571 Y_PRHEI_COMPRESSED_1541

    .define X_PRHEI_COMPRESSED_1581 X_PRHEI_COMPRESSED_1541
    .define Y_PRHEI_COMPRESSED_1581 Y_PRHEI_COMPRESSED_1541

.elseif FILESYSTEM = FILESYSTEMS::TRACK_SECTOR

    .define X_SHEBA_UNCOMPRESSED_1541 $01
    .define Y_SHEBA_UNCOMPRESSED_1541 $00

    .define X_PRHEI_UNCOMPRESSED_1541 $04
    .define Y_PRHEI_UNCOMPRESSED_1541 $0f


    .define X_SHEBA_UNCOMPRESSED_1571 $11
    .define Y_SHEBA_UNCOMPRESSED_1571 $00

    .define X_PRHEI_UNCOMPRESSED_1571 $3a
    .define Y_PRHEI_UNCOMPRESSED_1571 $05


    .define X_SHEBA_UNCOMPRESSED_1581 $27
    .define Y_SHEBA_UNCOMPRESSED_1581 $00

    .define X_PRHEI_UNCOMPRESSED_1581 $29
    .define Y_PRHEI_UNCOMPRESSED_1581 $23


    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH

        .define X_SHEBA_COMPRESSED_1541 $02
        .define Y_SHEBA_COMPRESSED_1541 $04

        .define X_PRHEI_COMPRESSED_1541 $06
        .define Y_PRHEI_COMPRESSED_1541 $13


        .define X_SHEBA_COMPRESSED_1571 $13
        .define Y_SHEBA_COMPRESSED_1571 $00

        .define X_PRHEI_COMPRESSED_1571 $3b
        .define Y_PRHEI_COMPRESSED_1571 $12


        .define X_SHEBA_COMPRESSED_1581 $27
        .define Y_SHEBA_COMPRESSED_1581 $20

        .define X_PRHEI_COMPRESSED_1581 $26
        .define Y_PRHEI_COMPRESSED_1581 $0f

    .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER

        .define X_SHEBA_COMPRESSED_1541 $03
        .define Y_SHEBA_COMPRESSED_1541 $08

        .define X_PRHEI_COMPRESSED_1541 $06
        .define Y_PRHEI_COMPRESSED_1541 $04


        .define X_SHEBA_COMPRESSED_1571 $13
        .define Y_SHEBA_COMPRESSED_1571 $0b

        .define X_PRHEI_COMPRESSED_1571 $3c
        .define Y_PRHEI_COMPRESSED_1571 $05


        .define X_SHEBA_COMPRESSED_1581 $29
        .define Y_SHEBA_COMPRESSED_1581 $00

        .define X_PRHEI_COMPRESSED_1581 $26
        .define Y_PRHEI_COMPRESSED_1581 $03

    .elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH

        .define X_SHEBA_COMPRESSED_1541 $03
        .define Y_SHEBA_COMPRESSED_1541 $14

        .define X_PRHEI_COMPRESSED_1541 $06
        .define Y_PRHEI_COMPRESSED_1541 $0a


        .define X_SHEBA_COMPRESSED_1571 $10
        .define Y_SHEBA_COMPRESSED_1571 $0b

        .define X_PRHEI_COMPRESSED_1571 $3c
        .define Y_PRHEI_COMPRESSED_1571 $0b


        .define X_SHEBA_COMPRESSED_1581 $29
        .define Y_SHEBA_COMPRESSED_1581 $0c

        .define X_PRHEI_COMPRESSED_1581 $26
        .define Y_PRHEI_COMPRESSED_1581 $09

    .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER

        .define X_SHEBA_COMPRESSED_1541 $04
        .define Y_SHEBA_COMPRESSED_1541 $0b

        .define X_PRHEI_COMPRESSED_1541 $07
        .define Y_PRHEI_COMPRESSED_1541 $10


        .define X_SHEBA_COMPRESSED_1571 $14
        .define Y_SHEBA_COMPRESSED_1571 $04

        .define X_PRHEI_COMPRESSED_1571 $3c
        .define Y_PRHEI_COMPRESSED_1571 $11


        .define X_SHEBA_COMPRESSED_1581 $29
        .define Y_SHEBA_COMPRESSED_1581 $18

        .define X_PRHEI_COMPRESSED_1581 $26
        .define Y_PRHEI_COMPRESSED_1581 $15

    .endif

.endif

; testing macros

.if VALIDATE

    .macro VALIDATE_PROLOGUE
            ; copy away
            jsr piccopy
    .endmacro

    .macro VALIDATE_EPILOGUE
            jsr piccomp
            jne validfail
    .endmacro

.endif

.macro TEST testmacro, param0, param1
    testmacro param0, param1
    jsr uninstchek
    .if VALIDATE
        VALIDATE_PROLOGUE
        testmacro param0, param1
        jsr uninstchek
        VALIDATE_EPILOGUE
    .endif
.endmacro

.macro TESTUNCOMPRESSED testmacro
    TEST testmacro, X_SHEBA_UNCOMPRESSED, Y_SHEBA_UNCOMPRESSED
    TEST testmacro, X_PRHEI_UNCOMPRESSED, Y_PRHEI_UNCOMPRESSED
.endmacro

.macro TESTCOMPRESSED testmacro
    TEST testmacro, X_SHEBA_COMPRESSED, Y_SHEBA_COMPRESSED
    TEST testmacro, X_PRHEI_COMPRESSED, Y_PRHEI_COMPRESSED
.endmacro

.macro CONSOLE text
            lda #.lobyte(text)
            ldy #.hibyte(text)
            jsr cout
.endmacro

.macro INITSTAT
            jsr initstat
.endmacro

.macro PRINTSTAT numbytes
            ldx #.hibyte(numbytes)
            ldy #.lobyte(numbytes)
            jsr printstat
.endmacro


.segment "CODE"

            sei
            ldx #$ff
            txs
            lda #MEMCONFIG_IO_KERNAL_BASIC
            sta IO_PORT

            lda #$00
            sta CIA1_TOD_HRS
            sta CIA1_TOD_MIN
            sta CIA1_TOD_SEC
            sta CIA1_TOD_10S

            ldx #$02
:           lda $00,x
            sta zpbuffer,x
            inx
            bne :-

            ; copy basic and kernal rom contents to ram
            lda #.hibyte(BASIC_ROM_SIZE)
            ldx #.hibyte(BASIC_ROM)
            ldy #.hibyte(BASIC_ROM)
            jsr memcopy
            lda #.hibyte(KERNAL_ROM_SIZE)
            ldx #.hibyte(KERNAL_ROM)
            ldy #.hibyte(KERNAL_ROM)
            jsr memcopy

            jsr waitretrace
:           bit VIC2_CTRL1
            bpl :-
            lda #$37
:           cmp VIC2_RASTERLINE
            bne :-
            lda VIC2_CTRL1
            asl
            lda #$00
            rol
            sta PALNTSC
            lsr
            lda #TOD_FREQ_60HZ
            bcc :+
            lda #TOD_FREQ_50HZ
:           sta CIA1_CRA

            jsr waitretrace
            lda #$00
            sta VIC2_CTRL1
            lda #($0400 >> 6) | CHARSET_UPPERLOWER
            sta VIC2_ADDR
            lda #COLOUR_BLACK
            sta VIC2_BGCOLOUR
            sta VIC2_BORDERCOLOUR
            jsr CLRSCR
            jsr waitretrace
            lda #TEXT_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3
            sta VIC2_CTRL1

            CONSOLE(startupmsg)

.if DYNLINKLDR
            CONSOLE(importmsg)

            PREPARE_IMPORT diskio

            lda #.lobyte(diskio)
            ldx #.hibyte(diskio)
            jsr importldr
            jcs error
            txa
            pha
            tya
            pha
            CONSOLE(donemsg)
            CONSOLE(quotemsg)
            pla
            tay
            pla
            jsr cout
            CONSOLE(quotemsg + 1)
            CONSOLE(returnmsg)
.endif

            lda #.lobyte(brkhandler)
            sta $0316
            lda #.hibyte(brkhandler)
            sta $0317

            CONSOLE(installmsg)

.if !NONBLOCKING_API
    .if LOAD_ONCE
            ldx #.lobyte(filename)
            ldy #.hibyte(filename)
    .endif
            jsr install
            stx DRIVETYPE
            cmp #diskio::status::OK
            beq :+
    .if (LOAD_VIA_KERNAL_FALLBACK && FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME)
            cmp #diskio::status::DEVICE_INCOMPATIBLE
            beq :+
        .if PROTOCOL = PROTOCOLS::TWO_BITS_ATN
            cmp #diskio::status::TOO_MANY_DEVICES
            beq :+
        .endif
    .endif
            jmp error
:
            tya
            pha
            CONSOLE(donemsg)
            CONSOLE(quotemsg)
            pla
            tax
            lda $00,x
            ldy $01,x
            jsr cout
            CONSOLE(quotemsg + 1)

    .if !LOAD_ONCE
            ldx DRIVETYPE
            cpx #diskio::drivetypes::DRIVE_1581 + 1
            bcc :+
            ldx #diskio::drivetypes::DRIVE_GENERIC
:           inx
            inx
            inx
            ldy paramoffst,x
            lda xshebaunpt,y
            sta X_SHEBA_UNCOMPRESSED
            lda yshebaunpt,y
            sta Y_SHEBA_UNCOMPRESSED

            lda xprheiunpt,y
            sta X_PRHEI_UNCOMPRESSED
            lda yprheiunpt,y
            sta Y_PRHEI_UNCOMPRESSED

        .if HAS_DECOMPRESSOR
            lda xshebapakt,y
            sta X_SHEBA_COMPRESSED
            lda yshebapakt,y
            sta Y_SHEBA_COMPRESSED

            lda xprheipakt,y
            sta X_PRHEI_COMPRESSED
            lda yprheipakt,y
            sta Y_PRHEI_COMPRESSED
        .endif
    .endif
            lda #MEMCONFIG_IO
            sta IO_PORT
.endif
            ldx #COLOUR_BLACK
            stx VIC2_BORDERCOLOUR
            stx VIC2_BGCOLOUR
:           lda #(COLOUR_MEDIUMGREY << 4) | COLOUR_DARKGREY
            sta $6000,x
            sta $6100,x
            sta $6200,x
            sta $6300,x
            lda #COLOUR_LIGHTGREY
            sta VIC2_COLOURRAM + $00,x
            sta VIC2_COLOURRAM + $0100,x
            sta VIC2_COLOURRAM + $0200,x
            sta VIC2_COLOURRAM + $0300,x
            lda #$00
            sta SPRITES + $00,x
            sta SPRITES + $0100,x
            inx
            bne :-

.if NONBLOCKING_API
            lda #$00
    .if LOAD_ONCE
            ldx #.lobyte(filename)
            ldy #.hibyte(filename)
    .endif
            jsr installnb
            stx DRIVETYPE
            cmp #diskio::status::OK
            beq :+
    .if LOAD_VIA_KERNAL_FALLBACK
            cmp #diskio::status::DEVICE_INCOMPATIBLE
            beq :+
        .if PROTOCOL = PROTOCOLS::TWO_BITS_ATN
            cmp #diskio::status::TOO_MANY_DEVICES
            beq :+
        .endif
    .endif
            jmp error
:
            cpx #diskio::drivetypes::DRIVE_1581 + 1
            bcc :+
            ldx #diskio::drivetypes::DRIVE_GENERIC
:
    .if !LOAD_ONCE
            inx
            inx
            inx
            ldy paramoffst,x
            lda xshebaunpt,y
            sta X_SHEBA_UNCOMPRESSED
            lda yshebaunpt,y
            sta Y_SHEBA_UNCOMPRESSED

            lda xprheiunpt,y
            sta X_PRHEI_UNCOMPRESSED
            lda yprheiunpt,y
            sta Y_PRHEI_UNCOMPRESSED

        .if HAS_DECOMPRESSOR
            lda xshebapakt,y
            sta X_SHEBA_COMPRESSED
            lda yshebapakt,y
            sta Y_SHEBA_COMPRESSED

            lda xprheipakt,y
            sta X_PRHEI_COMPRESSED
            lda yprheipakt,y
            sta Y_PRHEI_COMPRESSED
        .endif
    .endif; !LOAD_ONCE
.else
            lda #.lobyte(nmihandler)
            sta NMI_VECTORLO
            lda #.hibyte(nmihandler)
            sta NMI_VECTORHI
.endif; NONBLOCKING_API

:           bit VIC2_CTRL1
            bmi :-
:           bit VIC2_CTRL1
            bpl :-

            lda #.hibyte($2000)
            ldx #.hibyte(BITMAP)
            jsr memclear

            lda #$18
            sta $d000
            lda #$30
            sta $d002
            lda #$48
            sta $d004
            lda #$60
            sta $d006
            lda #$78
            sta $d008
            lda #$90
            sta $d00a
            lda #$a8
            sta $d00c
            lda #$c0
            sta $d00e
            lda #$fb
            sta $d001
            sta $d003
            sta $d005
            sta $d007
            sta $d009
            sta $d00b
            sta $d00d
            sta $d00f
            lda #%00000000
            sta $d010
            lda #%11111111
            sta $d015
            lda #$07
            sta $d027
            sta $d028
            sta $d029
            sta $d02a
            sta $d02b
            sta $d02c
            sta $d02d
            sta $d02e
            ldx #(SPRITES & $3fff) / $40
            stx $63f8
            inx
            stx $63f9
            inx
            stx $63fa
            inx
            stx $63fb
            inx
            stx $63fc
            inx
            stx $63fd
            inx
            stx $63fe
            inx
            stx $63ff

            lda #$3b
            sta $d011
            lda #$80
            sta $d018
            lda #VIC2_BANK2
            sta CIA2_PRA
            lda #.lobyte(irq0)
.if LOAD_VIA_KERNAL_FALLBACK
            sta $0314
.endif
            sta IRQ_VECTORLO
            lda #.hibyte(irq0)
.if LOAD_VIA_KERNAL_FALLBACK
            sta $0315
.endif
            sta IRQ_VECTORHI
            lda #.lobyte(brkhandler)
            sta $0316
            lda #.hibyte(brkhandler)
            sta $0317

            lda #$00
            sta $dc00
            lda #$80
            sta $d012
            lda #$01
            sta $d01a
            lda #$7f
            sta $dc0d
            bit $dc0d
            dec $d019
            cli

            ; plot the static settings to the sprites

            lda #.lobyte(plotsprchr)
            sta IBSOUT + 0
            lda #.hibyte(plotsprchr)
            sta IBSOUT + 1

            ldx #$00
            ldy #$00
            jsr setplotxy

            ldx #.lobyte(paltext)
            ldy #.hibyte(paltext)
            lda PALNTSC
            bne :+
            ldx #.lobyte(ntsctext)
            ldy #.hibyte(ntsctext)
:           jsr plottext

            lda #','
            jsr CHROUT
            lda #' '
            jsr CHROUT
            lda #'#'
            jsr CHROUT
            lda FA
            cmp #10
            bcc :+
            lda #'1'
            jsr CHROUT
            sec
            lda FA
            sbc #10
:           ora #'0'
            jsr CHROUT
            lda #'/'
            jsr CHROUT

            clc
            lda #$03
            adc DRIVETYPE
            tax
            lda drivtypesl,x
            ldy drivtypesh,x
            tax
            jsr plottext

            lda #','
            jsr CHROUT
            lda #' '
            jsr CHROUT
            lda #'$'
            jsr CHROUT

.if DYNLINKLDR
            lda diskio + diskio::IMPORT::RESIDENT_SIZE + 1
            jsr plothex
            lda diskio + diskio::IMPORT::RESIDENT_SIZE + 0
            jsr plothex
.else
            lda #.hibyte(__LOADER_SIZE__)
            jsr plothex
            lda #.lobyte(__LOADER_SIZE__)
            jsr plothex
.endif

.if !INSTALL_ONLY
testloop:

.if LOAD_ONCE

            INITSTAT

    .if LOAD_TO_API
            lda #.lobyte(BITMAP + $20)
            sta loadaddrl
            lda #.hibyte(BITMAP + $20)
            sta loadaddrh
            sec
    .else
            clc
    .endif

    .if NONBLOCKING_API
            ; --- load uncompressed non-blockingly ---
            jsr ploadfil
:           ; imagine some data processing here
            lda loadstatus
            cmp #diskio::status::BUSY
            beq :-
            cmp #diskio::status::OK
            jne error
    .elseif LOAD_COMPD_API
            ; --- load compressed ---
            jsr loadcompd
            jcs error
    .elseif LOAD_UNCOMPD_API
            ; --- load uncompressed ---
            jsr loaduncompd
            jcs error
    .elseif OPEN_FILE_POLL_BLOCK_API
            ; --- load polled ---
            jsr openfile
            jcs error
:           jsr pollblock
.if DYNLINKLDR
            php
            cmp #diskio::status::NOT_SUPPORTED
            jeq error
            plp
.endif
            bcc :-
            cmp #diskio::status::OK
            jne error

    .endif
            PRINTSTAT $2002
.else
; !LOAD_ONCE

.if OPEN_FILE_POLL_BLOCK_API

            ; --- load polled ---

    .macro LOADPOLLED xsource, ysource

            INITSTAT

        .if LOAD_TO_API
            lda #.lobyte(BITMAP + $20)
            sta loadaddrl
            lda #.hibyte(BITMAP + $20)
            sta loadaddrh
            sec
        .else
            clc
        .endif
            ldx xsource
            ldy ysource
            jsr openfile
            jcs error
:           ; imagine some data processing here
            bit CIA2_PRA
            bvc :-; wait until a block is ready to download
            jsr pollblock
        .if DYNLINKLDR
            php
            cmp #diskio::status::NOT_SUPPORTED
            jeq error
            plp
        .endif
            bcc :-
            cmp #diskio::status::OK
            jne error

            PRINTSTAT $2002
    .endmacro

            TESTUNCOMPRESSED LOADPOLLED
.endif

.if LOAD_UNCOMPD_API

            ; --- load uncompressed ---

    .macro LOADUNCOMPRESSED xsource, ysource

            INITSTAT

        .if LOAD_TO_API
            lda #.lobyte(BITMAP + $20)
            sta loadaddrl
            lda #.hibyte(BITMAP + $20)
            sta loadaddrh
            sec
        .else
            clc
        .endif
            ldx xsource
            ldy ysource
            jsr loaduncompd
            jcs error

            PRINTSTAT $2002
    .endmacro

            TESTUNCOMPRESSED LOADUNCOMPRESSED
.endif

.if UNINSTALL_RUNS_DINSTALL
            ; --- upload drivecode ---
    .macro RUNDRIVECODE
            .local upload1571
            .local upload1581
            .local postupload

            inc VIC2_BORDERCOLOUR
            lda DRIVETYPE
            cmp #diskio::drivetypes::DRIVE_1570
            beq upload1571
            cmp #diskio::drivetypes::DRIVE_1571
            beq upload1571
            cmp #diskio::drivetypes::DRIVE_1581
            beq upload1581

    .if DYNLINKLDR
        .if JUMP_TABLE
            UPLOAD_DRIVECODE_1541 drivecode41 - DYNLINKOVERHEAD
            jmp postupload
upload1571: UPLOAD_DRIVECODE_1571 drivecode71 - DYNLINKOVERHEAD
            jmp postupload
upload1581: UPLOAD_DRIVECODE_1581 drivecode81 - DYNLINKOVERHEAD
        .else
            UPLOAD_DRIVECODE_1541 drivecode41 - DYNLINKOVERHEAD + INSTALLJUMPTABLESIZE
            jmp postupload
upload1571: UPLOAD_DRIVECODE_1571 drivecode71 - DYNLINKOVERHEAD + INSTALLJUMPTABLESIZE
            jmp postupload
upload1581: UPLOAD_DRIVECODE_1581 drivecode81 - DYNLINKOVERHEAD + INSTALLJUMPTABLESIZE
        .endif
    .else
            UPLOAD_DRIVECODE_1541 drivecode41
            jmp postupload
upload1571: UPLOAD_DRIVECODE_1571 drivecode71
            jmp postupload
upload1581: UPLOAD_DRIVECODE_1581 drivecode81
    .endif

postupload:

:           bit CIA2_PRA
            bvs :-
            lda #CIA_SERIAL_DATA_IN_INPUT | CIA_SERIAL_CLK_IN_INPUT | CIA_SERIAL_DATA_OUT_INPUT | CIA_SERIAL_CLK_OUT_INPUT | CIA_SERIAL_ATN_OUT_INPUT | CIA_RS232_INPUT | CIA_VIC2_BANK_OUTPUT
            sta CIA2_DDRA

            dec VIC2_BORDERCOLOUR
    .endmacro

            RUNDRIVECODE
.endif

.if MEM_DECOMP_API

            ; --- load a compressed file, then decompress after loading

    .macro LOADANDMEMDECOMP xsource, ysource

            INITSTAT

        .if LOAD_UNCOMPD_API
            ldx xsource
            ldy ysource
            clc
            jsr loaduncompd
            jcs error
        .else
            ldx xsource
            ldy ysource
            clc
            jsr openfile
            jcs error
:           jsr pollblock
.if DYNLINKLDR
            php
            cmp #diskio::status::NOT_SUPPORTED
            jeq error
            plp
.endif
            bcc :-
            cmp #diskio::status::OK
            jne error
        .endif

        .if MEM_DECOMP_TO_API
            lda #.lobyte($3fe0)
            sta decdestl
            lda #.hibyte($3fe0)
            sta decdesth
            sec
        .else
            clc
        .endif
            lda loadaddrh
            ldx loadaddrl
            jsr memdecomp

            PRINTSTAT $2002
    .endmacro

            TESTCOMPRESSED LOADANDMEMDECOMP
.endif

.if NONBLOCKING_API

            ; --- load uncompressed non-blockingly ---

    .macro LOADUNCOMPDNONBLOCKING xsource, ysource

            INITSTAT

        .if LOAD_TO_API
            lda #.lobyte(BITMAP + $20)
            sta loadaddrl
            lda #.hibyte(BITMAP + $20)
            sta loadaddrh
            sec
        .else
            clc
        .endif
            ldx xsource
            ldy ysource
            jsr ploadfil
:           ; imagine some data processing here
            lda loadstatus
            cmp #diskio::status::BUSY
            beq :-
            cmp #diskio::status::OK
            jne error

            PRINTSTAT $2002
    .endmacro

            TESTUNCOMPRESSED LOADUNCOMPDNONBLOCKING
.endif

.if LOAD_COMPD_API

            ; --- load with decompression ---

    .macro LOADCOMPRESSED xsource, ysource
            INITSTAT

            ldx xsource
            ldy ysource
            clc
            jsr loadcompd
            jcs error

            PRINTSTAT $2002
    .endmacro

            TESTCOMPRESSED LOADCOMPRESSED
.endif
            jmp testloop

uninstchek:

.if PAUSEBETWEENLOADS
            lda #PAUSEBETWEENLOADS
            ldx #$00
            ldy #$00
:           dex
            bne :-
            dey
            bne :-
            sec
            sbc #$01
            bne :-
.endif

.if !UNINSTALL_API
            rts
.else
            inc $dc01; uninstall on keypress
            bne off
            rts

off:
    .if NONBLOCKING_API
            jsr uninstallnb
    .else
            jsr uninstall
    .endif
.endif; !UNINSTALL_API

.endif; !LOAD_ONCE

.else
            ; INSTALL_ONLY
    .if UNINSTALL_API
:           inc $dc01; uninstall on keypress
            beq :-

        .if NONBLOCKING_API
            jsr uninstallnb
        .else
            jsr uninstall
        .endif
    .endif; UNINSTALL_API
.endif

.if (UNINSTALL_API | LOAD_ONCE)
            lda #MSGUNINST
            jsr printmsg
.endif
            lda #COLOUR_GREEN
            sta VIC2_BORDERCOLOUR
            jmp *

waitretrace:
:           bit VIC2_CTRL1
            bmi :-
:           bit VIC2_CTRL1
            bpl :-
            rts

initstat:   lda #.lobyte($ffff)
            sta CIA1_TA_LO
            sta CIA1_TB_LO
            lda #.hibyte($ffff)
            sta CIA1_TA_HI            
            sta CIA1_TB_HI
            lda CIA1_CRA
            and #~TOD_FREQ_MASK
            ora #FORCE_LOAD | CONTINUOUS | TIMER_START
            sta CIA1_CRA
            lda #FORCE_LOAD | CONTINUOUS | COUNT_TA_UNDF | TIMER_START
            sta CIA1_CRB
            rts

printstat:  lda #~TIMER_STOP
            and CIA1_CRA
            sta CIA1_CRA
            lda #TIMER_STOP
            sta CIA1_CRB

            txa
            jsr GIVAYF; to FAC
            ldx #.lobyte(floatbuff)
            ldy #.hibyte(floatbuff)
            jsr MOV2M

            ; print kb/s figure
            ldx #$00
            ldy #$01
            jsr setplotxy

            lda POINTERS+$02; pointer to sprite bitmap
            pha
            lda POINTERS+$03
            pha
            jsr swapzp

            sec
            lda #$ff - 31; subtract overhead
            sbc CIA1_TA_LO
            sta numcycles + $00
            lda #$ff
            sbc CIA1_TA_HI
            sta numcycles + $01
            lda #$ff
            sbc CIA1_TB_LO
            sta numcycles + $02
            lda #$ff
            sbc CIA1_TB_HI
            sta numcycles + $03

            LONGTOFAC numcycles
            ldx #.lobyte(floatbuff2)
            ldy #.hibyte(floatbuff2)
            jsr MOV2M

            lda zpbuffer + PALNTSC
            jeq ntsckbs
            LONGTOFAC numccpal
            jmp :+
ntsckbs:    LONGTOFAC numccntsc

:           lda #.lobyte(floatbuff2)
            ldy #.hibyte(floatbuff2)
            jsr FDIVT
            lda #.lobyte(floatbuff)
            ldy #.hibyte(floatbuff)
            jsr FDIVT

            jsr DIV10; /= 10
            jsr DIV10; /= 10
            jsr DIV10; /= 10

            jsr FOUT
            jsr STRLIT
            pla
            sta POINTERS+$03
            pla
            sta POINTERS+$02

            ; remove leading space from number string
            ldy #STRDSC_LEN
            sec
            lda (STRDSC),y; string length -= 1
            sbc #$01
            sta (STRDSC),y
            iny; -> STRDSC_PTR
            clc
            lda (STRDSC),y; string pos += 1
            adc #$01
            sta (STRDSC),y
            sta statlen
            jsr STROUT+$03

            lda POINTERS+$02
            pha
            lda POINTERS+$03
            pha
            jsr swapzp
            pla
            sta POINTERS+$03
            pla
            sta POINTERS+$02

            ldx #.lobyte(kbstext)
            ldy #.hibyte(kbstext)
            jsr plottext

            sec
            lda #3*8; num chars
statlen = * + 1
            sbc #$00
            sbc #$05; length of " KB/s"
            tax
:           lda #' '
            jsr CHROUT
            dex
            bne :-
            rts

error:      sei
            pha
            lda #~MULTICOLOUR_MODE
            and VIC2_CTRL2
            sta VIC2_CTRL2
            pla

            ; print error message
            jsr printmsg

errorhalt:  lda #COLOUR_RED
            sta VIC2_BORDERCOLOUR
errorhaltl: ldx #$08
:           bit VIC2_CTRL1
            bpl :-
:           bit VIC2_CTRL1
            bmi :-
            dex
            bne :--
            lda #COLOUR_RED ^ COLOUR_BLACK
            eor VIC2_BORDERCOLOUR
            sta VIC2_BORDERCOLOUR
            jmp errorhaltl

swapzp:     ldx #$02
:           lda zpbuffer,x
            ldy $00,x
            sta $00,x
            tya
            sta zpbuffer,x
            inx
            bne :-
            rts

cout:       sta POINTERS+$00
            sty POINTERS+$01
            ldx #MEMCONFIG_IO_KERNAL_BASIC
            stx $01
            ldx #DEVICE_SCREEN
            stx DFLTO
            cli
consoletime = *+$01
            lda #$00
            beq :+
            lda #$00
            sta consoletime
            lda #PETSCII_LIGHTBLUE
            jsr CHROUT
            lda #'['
            jsr CHROUT
            lda CIA1_TOD_HRS
            jsr putconhex
            lda #'.'
            jsr CHROUT
            lda CIA1_TOD_MIN
            jsr putconhex
            lda #':'
            jsr CHROUT
            lda CIA1_TOD_SEC
            jsr putconhex
            bit CIA1_TOD_10S; to free the latch
            lda #']'
            jsr CHROUT
            lda #' '
            jsr CHROUT

:           ldy #$00
:           lda (POINTERS+$00),y
            beq :++
            pha
            jsr CHROUT
            pla
            cmp #PETSCII_RETURN
            bne :+
            inc consoletime
:           iny
            bne :--
:           sei
            lda #MEMCONFIG_IO_KERNAL
            sta $01
            rts

putconhex:  tax
            lsr
            lsr
            lsr
            lsr
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1 - 64
:           adc #'0'
            jsr CHROUT
            txa
            and #%00001111
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1 - 64
:           adc #'0'
            jmp CHROUT

printmsg:   stx printx
            sty printy

            tax
            lda #$00
            tay
:           sta BITMAP + $00,y
            sta BITMAP + $40,y
            iny
            bne :-

            lda errormsgsl,x
            sta POINTERS+$00
            lda errormsgsh,x
            sta POINTERS+$01
            lda #.lobyte(BITMAP)
            sta POINTERS+$02
            lda #.hibyte(BITMAP)
            sta POINTERS+$03

            lda #BITMAP_MODE
            and VIC2_CTRL1
            sta putchar + $01
            bne :+
            lda #($0400 >> 6) | CHARSET_UPPERLOWER
            sta VIC2_ADDR
            lda #.hibyte($0400)
            sta POINTERS+$03

:           lda #MEMCONFIG_CHARGEN
            sta IO_PORT

            txa
            pha
            cmp #MSGUNINST
            beq doprintmsg

            jsr puthex

            lda #'('
            jsr putchar
printx = *+1
            lda #$00
            jsr puthex
            lda #','
            jsr putchar
printy = *+1
            lda #$00
            jsr puthex
            lda #')'
            jsr putchar
            lda #':'
            jsr putchar
            lda #' '
            jsr putchar

doprintmsg: ldy #$00
            lda (POINTERS+$00),y
            beq :+
            jsr putchar
            inc POINTERS+$00
            bne doprintmsg
            inc POINTERS+$01
            bne doprintmsg
:           pla
            cmp #ERRBRKOPC
            bne :+

brkaddrhi = *+$01
            lda #$00
            jsr puthexnum
brkaddrlo = *+$01
            lda #$00
            jsr puthexnum
            lda #'.'
            jsr putchar

:           lda #MEMCONFIG_IO
            sta IO_PORT
            rts

puthex:     tax
            lda #'$'
            jsr putchar
            txa
puthexnum:  tax
            lsr
            lsr
            lsr
            lsr
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1 - 64
:           adc #'0'
            jsr putchar
            txa
            and #%00001111
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1 - 64
:           adc #'0'

putchar:    ldy #$00
            bne :+

            sta (POINTERS+$02),y
            inc POINTERS+$02
            rts

:           ldy #$00
            sty POINTERS+$05
            asl
            rol POINTERS+$05
            asl
            rol POINTERS+$05
            asl
            rol POINTERS+$05
            sta POINTERS+$04
            lda #.hibyte(ROM_CHARSET_UPLOW)
            adc POINTERS+$05
            sta POINTERS+$05
            ldy #$07
:           lda (POINTERS+$04),y
            sta (POINTERS+$02),y
            dey
            bpl :-
            clc
            lda #$08
            adc POINTERS+$02
            sta POINTERS+$02
            bcc :+
            inc POINTERS+$03
:           rts

            ; in: a - length in pages
            ;     x - destination hibyte
memclear:   stx POINTERS+$01
            ldy #$00
            sty POINTERS+$00
            tax
            tya
memclrlp:   sta (POINTERS+$00),y
            iny
            bne memclrlp
            inc POINTERS+$01
            dex
            bne memclrlp
            rts

            ; in: a - length in pages
            ;     x - source hibyte
            ;     y - destination hibyte
memcopy:    stx POINTERS+$01
            sty POINTERS+$03
            ldy #$00
            sty POINTERS+$00
            sty POINTERS+$02
            tax
memcpylp:   lda (POINTERS+$00),y
            sta (POINTERS+$02),y
            iny
            bne memcpylp
            inc POINTERS+$01
            inc POINTERS+$03
            dex
            bne memcpylp
            rts

.if VALIDATE
validfail:  lda #diskio::status::VERIFY_FAILED
            jmp error

piccopy:    lda #.hibyte($2000)
            ldx #.hibyte(BITMAP)
            ldy #.hibyte(COMPBMP)
            jmp memcopy

piccomp:    lda #.lobyte($1f40)
            sta POINTERS+$00
            lda #.hibyte($1f40)
            ldx #.hibyte(BITMAP)
            ldy #.hibyte(COMPBMP)

            ; in: POINTERS+$00 - lobyte of length
            ;     a - hibyte of length
            ;     x - memory area 1 hibyte
            ;     y - memory area 2 hibyte
memcomp:    stx POINTERS+$02
            sty POINTERS+$04
            ldy #$00
            sty POINTERS+$01
            sty POINTERS+$03
            tax
memcmplp:   lda (POINTERS+$01),y
            cmp (POINTERS+$03),y
            bne memcmpne
            dey
            bne memcmplp
            inc POINTERS+$02
            inc POINTERS+$04
            dex
            bne memcmplp
            ldy POINTERS+$00
memcmpl1:   lda (POINTERS+$01),y
            cmp (POINTERS+$03),y
            bne memcmpne
            dey
            bne memcmpl1
memcmpne:   rts

.endif; VALIDATE
            ; spriteplot subroutines
setplotxy:  lda #.lobyte(SPRITES)
            ldx #.hibyte(SPRITES)
            tya
            beq :+
            lda #.lobyte(SPRITES+$08*3)
            ldx #.hibyte(SPRITES+$08*3)
:           sta POINTERS+$02
            stx POINTERS+$03
            rts


plottext:   lda #MEMCONFIG_CHARGEN
            sta IO_PORT
            stx POINTERS+$00
            sty POINTERS+$01

plotloop:   ldy #$00
            sty POINTERS+$05
            lda (POINTERS+$00),y
            beq plotend
            jsr CHROUT
            jmp plotloop

plotend:    lda #MEMCONFIG_IO
            sta IO_PORT
            rts

plothex:    pha
            lsr
            lsr
            lsr
            lsr
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1
:           adc #'0'
            jsr CHROUT
            pla
            and #%00001111
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1
:           adc #'0'

            ; fall through

plotsprchr: sta POINTERS+$05
            txa
            pha
            tya
            pha
            ldy #MEMCONFIG_CHARGEN
            sty IO_PORT

            lda POINTERS+$05
            ldy #$00
            sty POINTERS+$05
            asl
            rol POINTERS+$05
            asl
            rol POINTERS+$05
            asl
            sta POINTERS+$04
            lda POINTERS+$05
            rol
            adc #.hibyte(CHARSET_ADDR_UPPERLOWER)
            sta POINTERS+$05

            ldx #$08
:           lda (POINTERS+$04),y
            sta (POINTERS+$02),y
            iny
            inc POINTERS+$02
            inc POINTERS+$02
            dex
            bne :-

            inc POINTERS+$00
            bne :+
            inc POINTERS+$01
:           lda POINTERS+$02
            sbc #$0e
            sta POINTERS+$02

            and #%00111111
            cmp #$03
            beq :+
            cmp #$03+$08*3
            bne :++
:           lda #$40-3-1
            adc POINTERS+$02
            sta POINTERS+$02
            bcc :+
            inc POINTERS+$03

:           lda #MEMCONFIG_IO
            sta IO_PORT
            pla
            tay
            pla
            tax
            rts

            ; irq handler to switch from single colour to multi colour in the middle of the screen

irq0:       pha
            lda $01
            pha
            lda #%00000010
            and $01
            bne :+
            txa
            pha
            tsx
            lda $0104,x
            and #FLAG_B
            jne brkhandler
            pla
            tax
:           lda #$35
            sta IO_PORT
            lda #$18
            sta $d016
            lda #$f9
            sta $d012
            lda #.lobyte(irq1)
.if LOAD_VIA_KERNAL_FALLBACK
            sta $0314
.endif
            sta $fffe
            lda #.hibyte(irq1)
.if LOAD_VIA_KERNAL_FALLBACK
            sta $0315
.endif
            sta $ffff
            dec $d019
            pla
            sta IO_PORT

.if LOAD_VIA_KERNAL_FALLBACK
            lda #%00000010
            and $01
            beq :+
            pla
            jmp $ea81
:
.endif
            pla
            rti

            ; irq handler to simulate a tune running in the lower border

irq1:       pha
            lda $01
            pha
            lda #%00000010
            and $01
            bne :+
            txa
            pha
            tsx
            lda $0104,x
            and #FLAG_B
            jne brkhandler
            pla
            tax
:           lda #$35
            sta IO_PORT
            lda #$33
            sta $d011
            inc $d020
            lda #$c0
            sec
            sbc #$01
            bne *-$03
            dec $d020
            lda #$3b
            sta $d011
            lda #$08
            sta $d016
            lda #$80
            sta $d012
            lda #.lobyte(irq0)
.if LOAD_VIA_KERNAL_FALLBACK
            sta $0314
.endif
            sta $fffe
            lda #.hibyte(irq0)
.if LOAD_VIA_KERNAL_FALLBACK
            sta $0315
.endif
            sta $ffff
            dec $d019
            pla
            sta IO_PORT
.if LOAD_VIA_KERNAL_FALLBACK
            lda #%00000010
            and $01
            beq :+
            pla
            jmp $ea81
:
.endif
            pla
            rti

nmihandler: lda #UNEXPCNMI
            jmp error

brkhandler: pla
            pla
            pla
            pla
            pla
            sec
            sbc #$02
            sta brkaddrlo
            pla
            sbc #$00
            sta brkaddrhi

            lda #ERRBRKOPC
            jmp error

.data

startupmsg: .byte PETSCII_WHITE, PETSCII_RETURN, "Loader test application", PETSCII_RETURN
            .byte "by Krill/Plush.", PETSCII_RETURN
            .byte "Loader version ", REPOSITORY_VERSION, PETSCII_RETURN, PETSCII_RETURN
            .byte 0

.if DYNLINKLDR
importmsg:  .byte PETSCII_YELLOW, "Importing loader functions... "
            .byte 0
.endif
installmsg: .byte PETSCII_YELLOW, "Installing loader... "
            .byte 0

donemsg:    .byte PETSCII_WHITE, "done.", PETSCII_RETURN
            .byte 0

quotemsg:   .byte PETSCII_YELLOW, $22, 0
returnmsg:  .byte PETSCII_RETURN, 0

paltext:    scrcode "PAL"
            .byte $00
ntsctext:   scrcode "NTSC"
            .byte $00

drivtypesl: .byte .lobyte(strgeneric)
            .byte 0,0
            .byte .lobyte(str1541), .lobyte(str1541c), .lobyte(str1541ii)
            .byte .lobyte(str1570), .lobyte(str1571), .lobyte(str1581)

drivtypesh: .byte .hibyte(strgeneric)
            .byte 0,0
            .byte .hibyte(str1541), .hibyte(str1541c), .hibyte(str1541ii)
            .byte .hibyte(str1570), .hibyte(str1571), .hibyte(str1581)

str1541:    scrcode "CBM1541"
            .byte $00
str1541c:   scrcode "CBM1541C"
            .byte $00
str1541ii:  scrcode "CBM1541II"
            .byte $00
str1570:    scrcode "CBM1570"
            .byte $00
str1571:    scrcode "CBM1571"
            .byte $00
str1581:    scrcode "CBM1581"
            .byte $00
strgeneric: scrcode "generic"
            .byte $00

kbstext:    scrcode " KB/s"
            .byte $00

.if LOAD_ONCE
filename:
    .if NONBLOCKING_API
            .asciiz "sb/sheba.bin"
    .elseif DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            .asciiz "sp/sheba.pu"
    .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER
            .asciiz "sv/sheba.bbconv"
    .elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
            .asciiz "sl/sheba.lcconv"
    .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
            .asciiz "se/sheba.exo"
    .else
            .asciiz "sb/sheba.bin"
    .endif
.elseif FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME

shebaunc:   .asciiz "sb/sheba.bin"
prheiunc:   .asciiz "pb/prlogo.bin"

    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
shebacompd: .asciiz "sp/sheba.pu"
prheicompd: .asciiz "pp/prlogo.pu"
    .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER
shebacompd: .asciiz "sv/sheba.bbconv"
prheicompd: .asciiz "pv/prlogo.bbconv"
    .elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
shebacompd: .asciiz "sl/sheba.lcconv"
prheicompd: .asciiz "pl/prlogo.lcconv"
    .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
shebacompd: .asciiz "se/sheba.exo"
prheicompd: .asciiz "pe/prlogo.exo"
    .endif

.endif

.if !LOAD_ONCE

paramoffst: .byte $00; generic drive
            .byte 0,0
            .byte $00; 1541
            .byte $00; 1541-C
            .byte $00; 1541-II
            .byte $00; 1570
            .byte $01; 1571
            .byte $02; 1581

xshebaunpt: .byte X_SHEBA_UNCOMPRESSED_1541
            .byte X_SHEBA_UNCOMPRESSED_1571
            .byte X_SHEBA_UNCOMPRESSED_1581
yshebaunpt: .byte Y_SHEBA_UNCOMPRESSED_1541
            .byte Y_SHEBA_UNCOMPRESSED_1571
            .byte Y_SHEBA_UNCOMPRESSED_1581
xprheiunpt: .byte X_PRHEI_UNCOMPRESSED_1541
            .byte X_PRHEI_UNCOMPRESSED_1571
            .byte X_PRHEI_UNCOMPRESSED_1581
yprheiunpt: .byte Y_PRHEI_UNCOMPRESSED_1541
            .byte Y_PRHEI_UNCOMPRESSED_1571
            .byte Y_PRHEI_UNCOMPRESSED_1581
    .if HAS_DECOMPRESSOR
xshebapakt: .byte X_SHEBA_COMPRESSED_1541
            .byte X_SHEBA_COMPRESSED_1571
            .byte X_SHEBA_COMPRESSED_1581
yshebapakt: .byte Y_SHEBA_COMPRESSED_1541
            .byte Y_SHEBA_COMPRESSED_1571
            .byte Y_SHEBA_COMPRESSED_1581
xprheipakt: .byte X_PRHEI_COMPRESSED_1541
            .byte X_PRHEI_COMPRESSED_1571
            .byte X_PRHEI_COMPRESSED_1581
yprheipakt: .byte Y_PRHEI_COMPRESSED_1541
            .byte Y_PRHEI_COMPRESSED_1571
            .byte Y_PRHEI_COMPRESSED_1581
    .endif
.endif

errormsgsl: .byte 0

            .byte .lobyte(emsgerread)
            .byte .lobyte(emsgerrhdr)
            .byte .lobyte(emsgerros)
            .byte .lobyte(emsgerrfmt)
            .byte .lobyte(emsgerrmem)

            .byte .lobyte(warndynlfb)

            .res $79,0
MSGUNINST = * - errormsgsl
            .byte .lobyte(emsguninst)
ERRBRKOPC = * - errormsgsl
            .byte .lobyte(emsgbrkopc)
UNEXPCNMI = * - errormsgsl
            .byte .lobyte(emsgunxnmi)
            .res $72,0

            .byte .lobyte(emsgversio)
            .byte .lobyte(emsgheader), .lobyte(emsgread)
            .byte .lobyte(everfailed), .lobyte(emsgnotsup)
            .byte .lobyte(emsgparerr), .lobyte(emsgdevnp), .lobyte(emsgdevinc), .lobyte(emsgtoomd), .lobyte(emsggenker)
            .byte .lobyte(eunspecerr)

errormsgsh: .byte 0
            .byte .hibyte(emsgerread)
            .byte .hibyte(emsgerrhdr)
            .byte .hibyte(emsgerros)
            .byte .hibyte(emsgerrfmt)
            .byte .hibyte(emsgerrmem)

            .byte .hibyte(warndynlfb)

            .res $79,0
            .byte .hibyte(emsguninst)
            .byte .hibyte(emsgbrkopc)
            .byte .hibyte(emsgunxnmi)
            .res $72,0

            .byte .hibyte(emsgversio)
            .byte .hibyte(emsgheader), .hibyte(emsgread)
            .byte .hibyte(everfailed), .hibyte(emsgnotsup)
            .byte .hibyte(emsgparerr), .hibyte(emsgdevnp), .hibyte(emsgdevinc), .hibyte(emsgtoomd), .hibyte(emsggenker)
            .byte .hibyte(eunspecerr)

.if VALIDATE
emsginval:  scrcode "Validate failed."
            .byte $00
.endif

emsguninst: scrcode "The loader is uninstalled."
            .byte $00
emsgbrkopc: scrcode "Executed BRK instruction at $"
            .byte $00
emsgunxnmi: scrcode "Unexpected NMI triggered."
            .byte $00

emsgerread: scrcode "Modload: Read error."
            .byte $00
emsgerrhdr: scrcode "Modload: Header error."
            .byte $00
emsgerros:  scrcode "Modload: Wrong OS."
            .byte $00
emsgerrfmt: scrcode "Modload: Data format error."
            .byte $00
emsgerrmem: scrcode "Modload: Not enough memory."
            .byte $00

warndynlfb: scrcode "Imported fallback library."
            .byte $00

emsgversio: scrcode "Wrong version."
            .byte $00

emsgheader: scrcode "Header error."
            .byte $00

emsgread:   scrcode "Read-only."
            .byte $00

everfailed: scrcode "Verify failed."
            .byte $00

emsgnotsup: scrcode "Not supported."
            .byte $00

emsgparerr:
.if FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME
            scrcode "File not found."
.elseif FILESYSTEM = FILESYSTEMS::TRACK_SECTOR
            scrcode "Illegal track or sector."
.endif
            .byte $00
emsgdevnp:  scrcode "Device not present."
            .byte $00
emsgdevinc: scrcode "Device incompatible."
            .byte $00
emsgtoomd:  scrcode "Too many devices."
            .byte $00
emsggenker: scrcode "Generic KERNAL error."
            .byte $00
eunspecerr: scrcode "Unspecified error."
            .byte $00

numccpal:   CYCLES_PER_SECOND_PAL
numccntsc:  CYCLES_PER_SECOND_NTSC

zpbuffer = * - 2
            .res 254
numcycles:  .res 4
floatbuff:  .res 5
floatbuff2: .res 5
