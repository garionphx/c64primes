
.ifndef _LOADER_INC_
_LOADER_INC_ = 1

; available protocols, file systems, and decompressors

.enum PROTOCOLS     ; the available transfer protocols - in this version, only TWO_BITS_ATN is implemented

   ;ONE_BIT         ; clocking every bit. allows for arbitrary interrupts, yet it's slower than TWO_BITS_ATN
                    ; and TWO_BITS but has no iec bus restrictions - not implemented yet    
   ;TWO_BITS        ; clocking every byte, the 8 bits within a byte are transferred strictly synchronously
                    ; and in groups of 2, no iec bus restrictions
                    ; the fastest protocol that allows for badlines, but does not allow for arbitrary
                    ; interrupts, so sprites are forbidden in the main loop, also sei and cli are used,
                    ; so interrupts are delayed for maximum 2 rasterlines - not implemented yet
   ;TWO_BITS_NOBL   ; same as TWO_BITS but without bad line checks, so bad lines are not allowed outside irq handlers.
                    ; the fastest available protocol. has the same drive-side routine as TWO_BITS. - not implemented yet
    TWO_BITS_ATN    ; the fastest protocol that allows for arbitrary interrupts (sprites, bad lines, etc.)
                    ; but it works with only 1 iec device on the bus.
.endenum

.enum FILESYSTEMS   ; the available file systems - in this version, only DIRECTORY_NAME, and TRACK_SECTOR is implemented

    DIRECTORY_NAME  ; operation on single files that are referenced using the directory, where the filenames
                    ; can be 1 to 16 chars long - no wildcard matching, so * and ? are treated as normal characters;
                    ; each file of the maximum 144 per 1541 disk side is accessible
    DIRECTORY_INDEX ; operation on single files that are identified by their position in the directory,
                    ; non-files (track 0) are counted
    TRACK_SECTOR    ; operation on single files that are referenced using track and sector position,
                    ; the filename does not have to appear in the directory
   ;DIRECTORY_HASH  ; operation on single files that are referenced using the directory, where the "filenames"
                    ; are a 16 bit hash number on the actual file names
   ;IFFL            ; operation on large iffl files (file databases in single files, one for each disk side)
                    ; where the files are referenced by indices into the iffl files. one big file looks cooler
                    ; than lots of small single files and also removes sector allocation overhead - not implemented yet
.endenum

.enum DECOMPRESSORS ; the available decompression routines - in this version, STREAM, EXOSTREAM and OXYPACKER are not implemented yet

    ;POLLING        ; query for ready-to-send blocks in the main program, do own processing until a block has
                    ; arrived, then download it from the drive, an optional optimized NMI-based polling system is provided (see below)
                    ; the POLLING "decompressor" is always available and cannot be explicitly selected

    STREAM          ; stream in data using a cyclic stream buffer, see below for options
    EXOSTREAM       ; Magnus Lind's Exomizer in streaming mode - no fast decompression,
                    ; but nearly optimal compression ratio (rle+lz+huffman), see below for options - not implemented yet -
                    ; compression ratio is dependent on the buffer size used

    EXOMIZER        ; Magnus Lind's Exomizer - no fast decompression, but nearly optimal compression ratio (rle+lz+huffman)
    PUCRUNCH        ; Pasi Ojala's Pucrunch - not so fast decompression but very good compression ratio (rle+lz+huffman)
    BYTEBOOZER      ; HCL/Booze Design's ByteBoozer - pretty fast decompression and good compression ratio (lz, a little better than Levelcrush)
    LEVELCRUSH      ; Taboo Levelcrush - pretty fast decompression and good compression ratio (lz)
    OXYPACKER       ; Graham/Oxyron's OxyPacker - very fast decompression and okay compression ratio (rle) - not implemented yet
    
    NONE            ; no decompressor used

.endenum


.ifdef EXTCONFIG
    .include "loaderconfig.inc"
.else
    .include "config.inc"
.endif; !EXTCONFIG

.if FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME
    .define INVALID_PARAMETERS FILE_NOT_FOUND
.elseif FILESYSTEM = FILESYSTEMS::DIRECTORY_INDEX
    .define INVALID_PARAMETERS INVALID_INDEX
.elseif FILESYSTEM = FILESYSTEMS::TRACK_SECTOR
    .define INVALID_PARAMETERS ILLEGAL_TRACK_OR_SECTOR
.endif

.include "diskio.inc"

.include "option-check.inc"

.ifdef DYNLINKLDR
    .if DYNLINKLDR
        .macro PREPARE_IMPORT request
            lda #.lobyte(__LOADERINSTALL_RUN__)
            sta request + diskio::IMPORT::INSTALL_BASE + 0
            lda #.hibyte(__LOADERINSTALL_RUN__)
            sta request + diskio::IMPORT::INSTALL_BASE + 1
            lda #.lobyte(__LOADERINSTALL_SIZE__)
            sta request + diskio::IMPORT::INSTALL_SIZE + 0
            lda #.hibyte(__LOADERINSTALL_SIZE__)
            sta request + diskio::IMPORT::INSTALL_SIZE + 1

            lda #.lobyte(__LOADER_RUN__)
            sta request + diskio::IMPORT::RESIDENT_BASE + 0
            lda #.hibyte(__LOADER_RUN__)
            sta request + diskio::IMPORT::RESIDENT_BASE + 1
            lda #.lobyte(__LOADER_SIZE__)
            sta request + diskio::IMPORT::RESIDENT_SIZE + 0
            lda #.hibyte(__LOADER_SIZE__)
            sta request + diskio::IMPORT::RESIDENT_SIZE + 1
        .endmacro
    .endif
.endif

.ifndef __NOIMPORTVARS
    .importzp loadstatus

    .importzp loadaddrl
    .importzp loadaddrh

    .importzp decdestl
    .importzp decdesth

    .importzp endaddrl
    .importzp endaddrh

    .importzp bytesloadedl
    .importzp bytesloadedh

    .importzp param4
    .importzp param5
.endif

.if UNINSTALL_RUNS_DINSTALL

    .include "cia.inc"

    .ifdef DYNLINKLDR
        .if DYNLINKLDR
            ; this is needed because this file is included by the app code
            ; before including the jumptable files, as .defines don't have
            ; effect on macro expansion
            .define __uninstall __LOADER_RUN__+_A_+_B_+_C_+_D_+_E_+_F_
        .else
            .define __uninstall uninstall
        .endif
    .else
        .define __uninstall uninstall
    .endif

    .macro __UPLOAD drivecode_address, drivebusy, driveprg

            .local uploadloop

            jsr __uninstall

            lda #CIA_SERIAL_DATA_IN_INPUT | CIA_SERIAL_CLK_IN_INPUT | CIA_SERIAL_DATA_OUT_INPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_RS232_OUTPUT | CIA_VIC2_BANK_OUTPUT
            sta CIA2_DDRA

            lda #.hibyte(drivecode_address)
            sta uploadloop+$02
            ldx #.lobyte(drivecode_address)
uploadloop: lda a:$0000,x
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
            bne uploadloop+$05
            inx
            bne :+
            inc uploadloop+$02
:           cpx #.lobyte(drivecode_address + drivebusy - driveprg)
            bne uploadloop
            ldy uploadloop+$02
            cpy #.hibyte(drivecode_address + drivebusy - driveprg)
            bne uploadloop
    .endmacro

    .macro UPLOAD_DRIVECODE_1541 drivecode_address
            __UPLOAD drivecode_address, drivebusy41, driveprg41
    .endmacro; UPLOAD_DRIVECODE

    .macro UPLOAD_DRIVECODE_1570 drivecode_address
            __UPLOAD drivecode_address, drivebusy71, driveprg71
    .endmacro; UPLOAD_DRIVECODE

    .macro UPLOAD_DRIVECODE_1571 drivecode_address
            __UPLOAD drivecode_address, drivebusy71, driveprg71
    .endmacro; UPLOAD_DRIVECODE

    .macro UPLOAD_DRIVECODE_1581 drivecode_address
            __UPLOAD drivecode_address, drivebusy81, driveprg81
    .endmacro; UPLOAD_DRIVECODE
.endif

.endif; _LOADER_INC_
