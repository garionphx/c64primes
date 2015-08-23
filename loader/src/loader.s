
.fileopt comment, "Loader resident code portion"
.fileopt compiler, "CA65"
.fileopt author, "Gunnar Ruthenberg"

.include "cpu.inc"
.include "cia.inc"

.include "loader.inc"

.importzp BLOCKDESTLO
.importzp BLOCKINDEX
.importzp LOADYBUF
.importzp YPNTRBUF

.importzp DECOMPVARS


.if LOAD_VIA_KERNAL_FALLBACK
    .importzp LOADDESTPTR
    .include "kernal.inc"
.endif

.macpack longbranch


.segment "LOADER"


; special block numbers
DEVICE_NOT_PRESENT = $00
LOAD_FINISHED      = $fe; loading finished successfully
LOAD_ERROR         = $ff; file not found or illegal t or s


; cia2 ddra ($dd02) definitions
CIA2_DDRA_COMMON   = CIA_SERIAL_DATA_IN_INPUT | CIA_SERIAL_CLK_IN_INPUT | CIA_VIC2_BANK_OUTPUT
                                                                                                                           ; effectively, this
; DATA OUT, CLK OUT, ATN out are low; RS232 is low                                                                         ; is the kernal flag
KERNAL_CIA2_DDRA   = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_RS232_OUTPUT

; DATA OUT, CLK OUT, ATN OUT are high; RS232 is high
CIA2_DDRA_IDLE     = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_INPUT  | CIA_SERIAL_CLK_OUT_INPUT  | CIA_SERIAL_ATN_OUT_INPUT  | CIA_RS232_INPUT

; DATA OUT, CLK OUT, ATN out are low; RS232 is high
CIA2_DDRA_WAKEUP   = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_INPUT  | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_RS232_INPUT

CLOCK              = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_OUTPUT                             | CIA_RS232_INPUT
CLOCK_ATN_HI       = CLOCK | CIA_SERIAL_ATN_OUT_INPUT ; 1st and 3rd bit pairs
CLOCK_ATN_LO       = CLOCK | CIA_SERIAL_ATN_OUT_OUTPUT; 2nd and 4th bit pairs


.if LOAD_VIA_KERNAL_FALLBACK
    KERNALFILENO   = $02

    .macro BRANCH_IF_INSTALLED to
                lda #KERNAL_CIA2_DDRA
                cmp CIA2_DDRA
                bne to
    .endmacro

    .macro BRANCH_IF_NOT_INSTALLED to
                lda #KERNAL_CIA2_DDRA
                cmp CIA2_DDRA
                beq to
    .endmacro
.endif

.ifdef DYNLINK
    .macro DYNLINKEXPORT function, label
        .byte function, .lobyte(label - base), .hibyte(label - base)
    .endmacro

    .segment "JUMPTABLE"
    .segment "DATA"

    .out "Exported dynamic link symbols:"

    .word endresijmptable - *

.else
    .ifdef RESIADDR
            .org RESIADDR - 2
            .word * + 2; load address
    .endif

    .if JUMP_TABLE
    .out "Jump table:"
    .endif
.endif

.include "loader-jumptable.inc"; this also checks sensibility of options
endresijmptable:


.if LOAD_COMPD_API
    BYTESTREAM = 1
.else
    BYTESTREAM = 0
.endif


.if (!(.defined(DYNLINK))) && (!LOAD_VIA_KERNAL_FALLBACK)
    .assert (* < $d000) || (* >= $e000), error, "***** Error: The loader code must not reside at $d000..$dfff, please make sure the LOADER segment does not overlap with that memory range. *****"
.endif

.macro SENDBYTE_IMPL
sendbyte:   ldx #$08
@0:         lsr
            nop
            pha
            lda CIA2_DDRA
            and #~CIA_SERIAL_DATA_OUT_OUTPUT
            bcs @1
            ora #CIA_SERIAL_DATA_OUT_OUTPUT
@1:         eor #CIA_SERIAL_CLK_OUT_OUTPUT
            sta CIA2_DDRA
            pla
            dex
            bne @0
.endmacro


            ; --- common routines, blocking ---

            ; start loading
            ; in: x - .lobyte(filename) or track (depends on FILESYSTEM setting in config.inc)
            ;     y - .hibyte(filename) or sector (depends on FILESYSTEM setting in config.inc)
.ifdef openfile
openfile2:
.else
openfile:
.endif
.if !LOAD_ONCE
            sty BLOCKINDEX
.endif

.if LOAD_TO_API
            lda #OPC_STA_ZP
            bcc :+
            lda #OPC_LDA_ZP
:           sta storeladrl
            sta storeladrh
.endif

.if LOAD_PROGRESS_API
            lda #$00
            sta bytesloadedl
            sta bytesloadedh
.endif

.if LOAD_UNDER_D000_DFFF
            lda IO_PORT
            sta memconfig+$01
.endif

.if LOAD_VIA_KERNAL_FALLBACK
            BRANCH_IF_INSTALLED nofallback

ldruninstl: lda IO_PORT
            sta kernaloff+$01
            lda #MEMCONFIG_IO_KERNAL_BASIC
            sta IO_PORT

    .if LOAD_ONCE

            ldx #KERNALFILENO
            jsr CHKIN

    .else

            stx namestrpos+$00
            sty namestrpos+$01
            ldx #$ff
:           inx
namestrpos = *+$01
            lda a:$00,x
            bne :-
            txa
            pha
            lda #KERNALFILENO
            ldx FA
            ldy #$00
            jsr SETLFS
            pla
            ldx namestrpos+$00
            ldy namestrpos+$01
            jsr SETNAM
            jsr OPEN
    .endif
            php
            jsr READST
            plp
            tax
            lda #diskio::status::GENERIC_KERNAL_ERROR
            jcs fopenfail; branch on error

            ldx #KERNALFILENO
            jsr CHKIN
            ; file not found is not detected at this point
            ; but the kernalgbyt function will return an error
            ; when trying to get the first file data byte
            ; (i.e., after "getting" the load address);
            ; the busy led will keep flashing
    .if LOAD_TO_API
            lda #OPC_STA_ZP
            cmp storeladrl
            beq :+
            lda loadaddrl
            sta LOADDESTPTR+$00
            lda loadaddrh
            sta LOADDESTPTR+$01
            jsr CHRIN
            jsr CHRIN
            jmp fopenokay
:
    .endif
            jsr CHRIN
kernalstrl: sta LOADDESTPTR+$00
            sta loadaddrl
            jsr CHRIN
kernalstrh: sta LOADDESTPTR+$01
            sta loadaddrh
            jmp fopenokay

nofallback:
.endif; LOAD_VIA_KERNAL_FALLBACK

.if IDLE_BUS_LOCK
            lda #VIC2_BANK_MASK; when the loader is idle, the user is
            and CIA2_PRA       ; allowed to write anything to $dd00
            sta CIA2_PRA       ; set it to a known and valid state here
.endif
            ldy #CIA2_DDRA_WAKEUP
            sty CIA2_DDRA

.if BYTESTREAM || END_ADDRESS_API
            lda #$00
           ;ldy #$1f
    .if BYTESTREAM
:           sta loadedtb,y; clear the bitfield denoting the blocks already loaded
            dey
            bpl :-
    .endif
    .if END_ADDRESS_API
            sta endaddrl
            sta endaddrh
    .endif
.endif

.if !LOAD_ONCE

:           bit CIA2_PRA; wait for the drive to be ready, it will drop the CLK line
            bvc :-

    .if FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME

            stx BLOCKDESTLO; pointer hibyte is already stored at BLOCKINDEX = BLOCKDESTLO + 1
            ldy #$00
:           lda (BLOCKDESTLO),y
            pha
            SENDBYTE_IMPL
            pla
            beq :+
            iny
            cpy #FILENAME_MAXLENGTH + 1
            bne :-
:
    .elseif FILESYSTEM = FILESYSTEMS::TRACK_SECTOR

            txa
            jsr sendbyte; send track
            lda BLOCKINDEX
            jsr sendbyte; send sector

    .endif

    .if LOAD_VIA_KERNAL_FALLBACK
            ; check whether the loader is still installed,
            ; if not, try kernal routines
            ldx #$06
:           dex
            bne :-
            bit CIA2_PRA
            bpl fopenokay
            lda #KERNAL_CIA2_DDRA
            sta CIA2_DDRA
            ldx BLOCKDESTLO + 0
            ldy BLOCKDESTLO + 1
            jmp ldruninstl
    .endif

.endif; !LOAD_ONCE

fopenokay:  clc
returnok:   lda #diskio::status::OK; $00
            tax; file descriptor, always 0 since this loader
               ; only supports one open file at a time
dorts:
fopenfail:  rts



.ifdef pollblock
pollblock2:
.else
pollblock:
.endif

.if LOAD_VIA_KERNAL_FALLBACK
            BRANCH_IF_INSTALLED getblnofbk

            ldx #$fe
kernalgblk: jsr kernalgbyt
            bcs :++
    .if LOAD_UNDER_D000_DFFF
            ldy #MEMCONFIG_ALL_RAM
            sty IO_PORT
    .endif
            ldy #$00
            sta (LOADDESTPTR),y
    .if LOAD_UNDER_D000_DFFF
            ldy #MEMCONFIG_IO_KERNAL_BASIC
            sty IO_PORT
    .endif
            inc LOADDESTPTR+$00
            bne :+
            inc LOADDESTPTR+$01
:           dex
            bne kernalgblk
            clc
:           rts

kernalgbyt: jsr READST
            bne :+
            jsr CHRIN
            clc
            rts
:           pha
            lda #KERNALFILENO
            jsr CLOSE
            jsr CLRCHN
    .if END_ADDRESS_API
            lda LOADDESTPTR+$00
            sta endaddrl
            lda LOADDESTPTR+$01
            sta endaddrh
    .endif
kernaloff:  lda #$00
            sta IO_PORT
            pla
            cmp #%01000000
            bne kernalerr

    .if LOAD_PROGRESS_API
            inc bytesloadedl
            bne :+
            inc bytesloadedh
:
    .endif
            sec
            lda #diskio::status::OK
            rts
kernalerr:  sec
            tax
            bpl :+
            lda #diskio::status::DEVICE_NOT_PRESENT
            rts
:           jsr READST
            tax
            lda #diskio::status::GENERIC_KERNAL_ERROR
            sec
            rts
getblnofbk:

.endif; LOAD_VIA_KERNAL_FALLBACK

            jsr getblock
            bcs pollerr
.if OPEN_FILE_POLL_BLOCK_API || NONBLOCKING_API
            lda #diskio::status::CHANGING_TRACK
            bit CIA2_PRA
            bmi dorts
.endif; OPEN_FILE_POLL_BLOCK_API || NONBLOCKING_API
           ;clc
            bcc returnok

            ; accu is DEVICE_NOT_PRESENT ($00), LOAD_FINISHED ($fe, file loaded successfully), or LOAD_ERROR ($ff, file not found or illegal t or s) here
pollerr:
.if LOAD_ONCE
            ; uninstall
            ldx #KERNAL_CIA2_DDRA
.else
            ldx #CIA2_DDRA_IDLE
.endif
            stx CIA2_DDRA

            cmp #LOAD_FINISHED
            beq returnok; returns with carry set
            clc
            ; accu = $ff (LOAD_ERROR) -> diskio::status::INVALID_PARAMETERS
            ; accu = $00 (DEVICE_NOT_PRESENT) -> diskio::status::DEVICE_NOT_PRESENT
            adc #diskio::status::DEVICE_NOT_PRESENT
pollfail:   sec
polldone:   rts

            .assert diskio::status::DEVICE_NOT_PRESENT - diskio::status::INVALID_PARAMETERS = 1, error, "invalid code optimization"

getblock:   lda #DEVICE_NOT_PRESENT
            ldx #CLOCK_ATN_HI
:           bit CIA2_PRA
            bvc :-
            bmi pollfail; branch if device not present

            stx CIA2_DDRA; send atn strobe to the drive
            jsr getblkrts - $01; clc : rts - waste some time to make sure the drive is ready
            lda #OPC_RTS
            sta blockrts - CLOCK_ATN_HI,x
            lda #CLOCK_ATN_LO
            sta CIA2_DDRA - CLOCK_ATN_HI,x; end of atn strobe

            jsr get1byte; get block number

.if BYTESTREAM
            pha
            jsr loadedsb+$00
            ora loadedtb,y
            sta loadedtb,y; mark this block as loaded
            pla
.endif
            bne not1stblk

firstblock: jsr get1byte; block size
            pha
            jsr get1byte; load address lo
storeladrl: sta loadaddrl
            sta BLOCKDESTLO
            jsr get1byte; load address hi
storeladrh: sta loadaddrh
            sta storbyte+$02
            pla
            sec
            sbc #$02
            bcs fin1stblk; jmp

not1stblk:  cmp #LOAD_FINISHED; check for special block numbers: LOAD_FINISHED ($fe, loading finished successfully), LOAD_ERROR ($ff)
            bcs polldone

            ; calculate the position in memory according to the block number
            sta BLOCKINDEX
            lda loadaddrl
           ;clc
            sbc BLOCKINDEX
            php
            clc
            sbc BLOCKINDEX
            sta BLOCKDESTLO
            lda loadaddrh
            sbc #$00
            plp
            sbc #$01
            adc BLOCKINDEX
            sta storbyte+$02

            jsr get1byte; block size

fin1stblk:
.if LOAD_UNDER_D000_DFFF
            ldy #OPC_LDX_IMM
            sty blockrts
.else
            asl blockrts; change from $60 (OPC_RTS) to $c0 (OPC_CPY_IMM), clear carry
.endif

.if LOAD_PROGRESS_API
            pha
            sec
            adc bytesloadedl
            sta bytesloadedl
            bcc :+
            inc bytesloadedh
:           pla
            clc
.else
    .if LOAD_UNDER_D000_DFFF
            clc
    .endif
.endif

            adc BLOCKDESTLO
get1byte:   ldy BLOCKDESTLO
            sta loadycomp+$01
            clc
            bcc :+

            ; getblock loop, nominal run time per byte is 87 cycles,
            ; this is about 11.32 KB/s on PAL for mere transfer
getbklup:   iny
            bne :+

            inc storbyte+$02

:           ldx #CLOCK_ATN_HI
            lda CIA2_PRA
            stx CIA2_DDRA; ATN high        ; 4
            lsr                            ; 2
            lsr                            ; 2
            php                            ; 3
            nop                            ; 2
            ldx #CLOCK_ATN_LO              ; 2
                                           ; = 15

            ora CIA2_PRA
            stx CIA2_DDRA; ATN low         ; 4
            lsr                            ; 2
            lsr                            ; 2
            plp                            ; 4
            eor #RS232_OUT                 ; 2
            ldx #CLOCK_ATN_HI              ; 2
                                           ; = 16

            eor CIA2_PRA
            stx CIA2_DDRA; ATN high        ; 4
            lsr                            ; 2
            lsr                            ; 2
            sta :+ + $01                   ; 4
            ldx #CLOCK_ATN_LO              ; 2+1
                                           ; = 15

            lda CIA2_PRA - CLOCK_ATN_LO,x
            stx CIA2_DDRA; ATN low
            and #SERIAL_DATA_IN | SERIAL_CLK_IN
:           ora #$00
.if LOAD_UNDER_D000_DFFF

blockrts:   ldx #MEMCONFIG_ALL_RAM
            stx IO_PORT
storbyte:   sta a:$00,y
memconfig:  ldx #MEMCONFIG_IO
            stx IO_PORT
loadycomp:  cpy #$00

.else

blockrts:
loadycomp:  cpy #$00
storbyte:   sta a:$00,y; lobyte is always $00

.endif
            bne getbklup

.ifdef DYNLINKLDR
            .assert .hibyte(* + 1) = .hibyte(getbklup), warning, "***** Performance warning: page boundary crossing (getbylup). Please relocate the LOADER segment a few bytes up or down. *****"
.endif

.if END_ADDRESS_API
            cpy endaddrl
            lda storbyte+$02
            tax
            sbc endaddrh
            bcc :++
            iny
            sty endaddrl
            bne :+
            inx
:           stx endaddrh
:
.endif
            clc
getblkrts:  rts

.if FILESYSTEM <> FILESYSTEMS::DIRECTORY_NAME
            SENDBYTE_IMPL
            rts
.endif

.if BYTESTREAM

loadedsb:   tay
            and #%00000111
            tax
            tya
            lsr
            lsr
            lsr
            tay
            lda loadedor,x
            rts

loadedor:   .byte $01,$02,$04,$08; or-values for the bitfield
            .byte $10,$20,$40,$80

loadedtb:   .res 32,0; bitfield for already-loaded blocks, 256 bits for 64 kB minus 256*2 + 2 = 514 bytes memory

.endif; BYTESTREAM

.if UNINSTALL_API
    .ifdef uninstall
uninstall2:
    .else
uninstall:
    .endif
            lda #KERNAL_CIA2_DDRA
            sta CIA2_DDRA
    .if !UNINSTALL_RUNS_DINSTALL
:           bit CIA2_PRA
            bpl :-
            bvc :-
    .else
:           bit CIA2_PRA
            bvc :-
    .endif
            rts
.endif; UNINSTALL_API

.if LOAD_UNCOMPD_API

            ; --- load file without decompression ---

    .ifdef loaduncompd
loaduncompd2:
    .else
loaduncompd:
    .endif
            jsr openfile
    .if LOAD_VIA_KERNAL_FALLBACK
            bcs openerror
    .endif

:
    .if JUMP_TABLE && OPEN_FILE_POLL_BLOCK_API && (!NONBLOCKING_API) && (!.defined(DYNLINK))
            jsr pollblock2
    .else
            jsr pollblock
    .endif
            bcc :-
            cmp #diskio::status::OK + 1
    .if LOAD_VIA_KERNAL_FALLBACK
openerror:
    .endif
            rts

.endif; LOAD_UNCOMPD_API

.if MEM_DECOMP_API

            ; --- decompress a compressed file from memory ---
            ; in: a - >source
            ;     x - <source
            ;     destination address is stored in the compressed file's header

.ifdef memdecomp
memdecomp2:
.else
memdecomp:
.endif

            stx getmembyte+$01
    .if LOAD_COMPD_API
            jsr putloadb
    .else
            sta getmembyte+$02
    .endif

    .if CHAINED_COMPD_FILES
            jmp :+
        .ifdef cmemdecomp
cmemdecomp2:
        .else
cmemdecomp:
        .endif
            jsr getmembyte
            jsr getmembyte
:
    .endif

    .if MEM_DECOMP_TO_API
        .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            lda #OPC_BIT_ZP
            bcc :+
            lda #OPC_LDA_ZP
:           sta storedadrl
            sta storedadrh
        .else
            lda #OPC_STA_ZP
            bcc :+
            lda #OPC_LDA_ZP
:           sta storedadrl
            sta storedadrh
        .endif
    .endif
            jmp decompress

.endif; MEM_DECOMP_API

.if LOAD_COMPD_API

            ; --- load a compressed file ---
    .ifdef loadcompd
loadcompd2:
    .else
loadcompd:
    .endif
            jsr openfile
    .if LOAD_VIA_KERNAL_FALLBACK
            bcc :+
            rts
:
    .endif
            tsx
            stx stackpntr+$01

    .if MEM_DECOMP_TO_API
        .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            lda #OPC_BIT_ZP
            sta storedadrl
            sta storedadrh
        .else
            lda #OPC_STA_ZP
            sta storedadrl
            sta storedadrh
        .endif
    .endif

    .if LOAD_VIA_KERNAL_FALLBACK
            BRANCH_IF_INSTALLED nodeploadf

            lda #<kernalgcby
            ldx #>kernalgcby
            jsr puttoloadb
        .if LOAD_UNDER_D000_DFFF
            lda #MEMCONFIG_ALL_RAM
            sta IO_PORT
        .endif
        .if CHAINED_COMPD_FILES
            jmp :++
:           jsr kernalgcby; skip load address
            jsr kernalgcby
:           jsr decompress
            jmp :--
        .else
            jsr decompress
:           jsr kernalgcby
            bcc :-
        .endif
nodeploadf:
    .endif
            ldx #$00
            stx getmembyte+$01
            stx getmembyte+$02
            stx getdbyte+$02
            stx incblkpt+$01
            dex
            stx YPNTRBUF
            lda #<loadbyte
            ldx #>loadbyte
            jsr puttoloadb

    .if LOAD_UNDER_D000_DFFF
            lda #MEMCONFIG_ALL_RAM
            sta IO_PORT
    .endif

    .if CHAINED_COMPD_FILES
            jmp jsrdecomp

getstrbyte:
        .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            jmp (toloadbt+$01)
        .else
            jmp (toloadb0+$01)
        .endif

decomploop: jsr getstrbyte; skip load address
            jsr getstrbyte
jsrdecomp:
            jsr decompress
dependlo:   lda #$00
            cmp getmembyte+$01
dependhi:   lda #$00
            sbc getmembyte+$02
            bcs decomploop; decompress all compressed sub-files that may be inside the compressed file
    .else
            jsr decompress
    .endif

            lda getmembyte+$02
            bne :+
            jsr getblock; handle special case that decompressing is as quick as loading,
                        ; this call will fetch the loading finished flag and ack loading
:
            lda #diskio::status::OK

            ; loading and decompressing is finished
loadaret:   cmp #diskio::status::OK + 1; sets carry flag according to the error state
stackpntr:  ldx #$00
            txs
    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            ldx lo+$01
            ldy hi+$01
    .endif
            pha
    .if LOAD_UNDER_D000_DFFF
            lda memconfig+$01
            sta IO_PORT
    .endif
            pla; pha:pla for flags
            rts

    .if LOAD_VIA_KERNAL_FALLBACK
kernalgcby:
        .if LOAD_UNDER_D000_DFFF
            lda #MEMCONFIG_IO_KERNAL_BASIC
            sta IO_PORT
        .endif
            sty LOADYBUF
            jsr kernalgbyt
            ldy LOADYBUF
        .if LOAD_UNDER_D000_DFFF
            pha
            lda #MEMCONFIG_ALL_RAM
            sta IO_PORT
            pla
        .endif
            bcs loadaret
            rts
    .endif; LOAD_VIA_KERNAL_FALLBACK
.endif; LOAD_COMPD_API

.if HAS_DECOMPRESSOR
:           inc getmembyte+$02
            rts
getmembyte: lda a:$00
            inc getmembyte+$01
            beq :-
            rts; one extra byte for one cycle less
.endif

.if BYTESTREAM
            ; get a byte from the compressed file's byte-stream
loadbyte:   sty LOADYBUF

loadbyte2:  ldy YPNTRBUF
getdbyte:   lda a:$00,y
    .if LOAD_UNDER_D000_DFFF
            ldy #MEMCONFIG_IO
            sty IO_PORT
    .endif
            inc YPNTRBUF
            beq incblkpt
            bit CIA2_PRA
            bvs getnewblk
    .if LOAD_UNDER_D000_DFFF
loadbytret: ldy #MEMCONFIG_ALL_RAM
            sty IO_PORT
    .endif
            ldy LOADYBUF
            rts

firstblk:   clc
            lda BLOCKDESTLO  ; load address lo
            pha
            sbc loadycomp+$01; last byte of block lo
            sta YPNTRBUF     ; $0100-block size
            sec
            pla              ; load address lo
            sbc YPNTRBUF     ; +block size
            sta getdbyte+$01
            php
            lda loadycomp+$01; last byte of block lo
            cmp BLOCKDESTLO  ; load address lo
            lda storbyte+$02 ; load address hi
            sbc #$00
            plp
            sbc #$00
            sta getdbyte+$02
            inc incblkpt+$01
    .if LOAD_UNDER_D000_DFFF
            lda #MEMCONFIG_ALL_RAM
            sta IO_PORT
    .endif
            bne loadbyte2; branches always

incblkpt:   ldy #$00
            stx xbuf+$01
            bne checklod
            SKIPBYTE
wait4blk:   pla
            jsr getnewblk2
    .if LOAD_UNDER_D000_DFFF
            ldy #MEMCONFIG_IO
            sty IO_PORT
    .endif
            ldy incblkpt+$01
            beq firstblk
checklod:   pha
            tya
            jsr loadedsb+$01
            and loadedtb,y
            beq wait4blk; branch if the next block in the stream is not yet loaded

            clc
            lda #$fe
            adc getdbyte+$01
            sta getdbyte+$01
            bcc :+
            inc getdbyte+$02
:           lda #$02
            sta YPNTRBUF
            inc incblkpt+$01
            bne xbuf+$00

getnewblk:  stx xbuf+$01
getnewblk2: pha
            jsr getblock
            bcs lastblok; branch if block number is DEVICE_NOT_PRESENT ($00), LOAD_FINISHED ($fe), LOAD_ERROR ($ff, file not found or illegal t or s)
    .if CHAINED_COMPD_FILES
            ldx loadycomp+$01
            cpx dependlo+$01
            ldy storbyte+$02
            tya
            sbc dependhi+$01
            bcc xbuf
            stx dependlo+$01
            sty dependhi+$01
            inx
            stx getmembyte+$01
            bne :+
            iny
:           sty getmembyte+$02
    .endif
xbuf:       ldx #$00
            pla

    .if LOAD_UNDER_D000_DFFF
            jmp loadbytret
    .else
            ldy LOADYBUF
            rts
    .endif

lastblok:   pha; block number
            clc
            lda YPNTRBUF
            adc getdbyte+$01
            sta getmembyte+$01
            lda YPNTRBUF
            bne :+
            sec
:           lda #$00
            adc getdbyte+$02
            jsr putloadb
            pla
            jsr pollerr ; if accu is $00 (device not present), or $ff (file not found or illegal t or s),
                        ; return with an error, otherwise continue
            bcc xbuf+$00
            cmp #diskio::status::OK
            beq xbuf+$00
            ; an error occured, stop loading and decompressing, return error to the caller
            jmp loadaret

putloadb:   sta getmembyte+$02
            lda #<getmembyte
            ldx #>getmembyte
puttoloadb:
    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            sta toloadbt+$01
            stx toloadbt+$02
    .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER
            sta toloadb0+$01
            stx toloadb0+$02
            sta toloadb1+$01
            stx toloadb1+$02
            sta toloadb2+$01
            stx toloadb2+$02
            sta toloadb3+$01
            stx toloadb3+$02
            sta toloadb4+$01
            stx toloadb4+$02
    .elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
            sta toloadb0+$01
            stx toloadb0+$02
            sta toloadb1+$01
            stx toloadb1+$02
            sta toloadb2+$01
            stx toloadb2+$02
            sta toloadb3+$01
            stx toloadb3+$02
    .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
            sta toloadb0+$01
            stx toloadb0+$02
            sta toloadb1+$01
            stx toloadb1+$02
            sta toloadb2+$01
            stx toloadb2+$02
            sta toloadb3+$01
            stx toloadb3+$02
            sta toloadb4+$01
            stx toloadb4+$02
    .endif
            rts
.endif; BYTESTREAM

.if NONBLOCKING_API

    .ifdef ploadfil
ploadfil2:
    .else
ploadfil:
    .endif
            ; in: a - block fetch time slice when downloading a block in n*8 rasterlines ($00: get a whole block at a time) XXX TODO
            ;     x - main loop time slice when downloading a block in n*8 rasterlines (x = $00 or a = $00: none) XXX TODO
            lda loadstatus
            bne pnmi0-$01

            lda #.lobyte(pnmi0)
    .if LOAD_VIA_KERNAL_FALLBACK
            sta NMINV+$00
    .endif
            sta NMI_VECTORLO
            lda #.hibyte(pnmi0)
    .if LOAD_VIA_KERNAL_FALLBACK
            sta NMINV+$01
    .endif
            sta NMI_VECTORHI

            jsr openfile
            php
            pla
            ldx #OPC_NOP
            and #FLAG_I
            bne :+
            ; irqs are enabled
            lda #OPC_BNE
            ldx #OPC_CLI
            SKIPWORD
:           lda #OPC_BIT_ZP; irqs are disabled
            sta pseiclisw1
            stx pseiclisw2
            lda #POLLINGINTERVAL_STARTLOAD - $01
            sta CIA2_TB_LO
            lda #diskio::status::BUSY
            sta loadstatus
            lda #CIA_SET_INTF | TIMERB_IRQ
            sta CIA2_ICR
            bit CIA2_ICR
            rts

pnmi0:      bit CIA2_PRA
            bvs ppoll; branch if block is ready to download


    .if LOAD_VIA_KERNAL_FALLBACK
            pha
            BRANCH_IF_NOT_INSTALLED :+
            pla
    .endif
            bit CIA2_ICR
            rti


    .if LOAD_VIA_KERNAL_FALLBACK
:           pla
    .endif
ppoll:      bit loadstatus
            bvc pnopoll - $00
            sta PACCUBUF
            lda #POLLINGINTERVAL_REPOLL - $01
            sta CIA2_TB_LO
            pla
            pha
            and #FLAG_I
pseiclisw1: bne pnopoll - $02; don't download the block yet if the i-flag was set, i.e., if an irq handler was being executed
pseiclisw2: cli
            txa
            pha
            tya
            pha
            lda #CIA_CLR_INTF | $7f
            sta CIA2_ICR
            jsr pollblock
            bcs pendload
            cmp #diskio::status::CHANGING_TRACK
            beq ptrkchan
            lda #.lobyte(pnmi1)
    .if LOAD_VIA_KERNAL_FALLBACK
            sta NMINV+$00
    .endif
            sta NMI_VECTORLO
            lda #.hibyte(pnmi1)
    .if LOAD_VIA_KERNAL_FALLBACK
            sta NMINV+$01
    .endif
            sta NMI_VECTORHI

            lda #POLLINGINTERVAL_GETBLOCK - $01
            SKIPWORD
ptrkchan:   lda #POLLINGINTERVAL_TRACKCHANGE - $01
            sta CIA2_TB_LO
            lda #CIA_SET_INTF | TIMERB_IRQ
            sta CIA2_ICR

pcontlod:   pla
            tay
            pla
            tax
            lda PACCUBUF
pnopoll:    bit CIA2_ICR
            rti
pendload:   sta loadstatus
            lda #CIA_CLR_INTF | $7f
            sta CIA2_ICR
            bne pcontlod

pnmi1:      pha
            lda #.lobyte(pnmi0)
    .if LOAD_VIA_KERNAL_FALLBACK
            sta NMINV+$00
    .endif
            sta NMI_VECTORLO
            lda #.hibyte(pnmi0)
    .if LOAD_VIA_KERNAL_FALLBACK
            sta NMINV+$01
    .endif
            sta NMI_VECTORHI
            lda #POLLINGINTERVAL_BLOCKSOON - $01
            sta CIA2_TB_LO
            pla
            bit CIA2_ICR
            rti

    .if UNINSTALL_API
        .ifdef uninstallnb
uninstallnb2:
        .else
uninstallnb:
        .endif
            ; disable cia timer nmis
            lda #CIA_CLR_INTF | $7f
            sta CIA2_ICR
            bit CIA2_ICR
            jmp uninstall
    .endif
.endif; NONBLOCKING_API

.if HAS_DECOMPRESSOR
    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH

        .include "pudecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER

        .include "bbdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH

        .include "lcdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER

        FORWARD_DECRUNCHING = 1

        get_crunched_byte = getmembyte

        .include "exodecomp.s"

        decompress = decrunch

    .else
        .error "***** Error: the selected decompressor option is not yet implemented. *****"
    .endif
.endif

.ifndef DYNLINK
    .if LOAD_VIA_KERNAL_FALLBACK
        .assert * < $d000, error, "***** Error: the loader code must not exceed $d000, please make sure the LOADER segment ends below $d000. *****"
    .else
        .assert (* < $d000) || (* >= $e000), error, "***** Error: the loader code must not reside at $d000..$dfff, please make sure the LOADER segment does not overlap with that memory range. *****"
    .endif
.endif
