
; check configuration sensibility and define dynlink table

.include "loader.inc"
.include "option-check.inc"
.include "common-features.inc"

;------------------------------------------------------------------------------

.macro DEFAULT_INTERRUPT_RESIFEATS
    ; screen on, interrupts may be delayed, no nmis allowed, no sprites in non-irq space, is usually sufficient for applications
    .byte diskio::INTERRUPT_CONSTRAINTS, diskio::INTERRUPTS_POSSIBLE
    .out "  interrupts may be delayed, no sprites or interrupts used"
.endmacro

.macro INTERRUPT_RESIFEATS
    .ifdef DYNLINKLDR
        .ifdef USER_INTERRUPT_RESIFEATS
            .if USER_INTERRUPT_RESIFEATS <> 0
                USER_INTERRUPT_RESIFEATS_IMPL
            .else
                DEFAULT_INTERRUPT_RESIFEATS
            .endif
        .else
            DEFAULT_INTERRUPT_RESIFEATS
        .endif
    .else
        .byte diskio::INTERRUPT_CONSTRAINTS, diskio::ARBITRARY_INTERRUPTS
        .out "  screen on, no interrupt delays, nmis allowed, no sprite limitations"
    .endif
.endmacro

;------------------------------------------------------------------------------

.if HAS_DECOMPRESSOR
    .macro DEFAULT_COMPRESSION_RESIFEATS
        .byte diskio::COMPRESSION
        .if DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
            .if LC_SPEED = 0
                .byte diskio::TABOO_LEVELCRUSH_SPEED_0
                .out "  decompression with Taboo Levelcrush at speed 0"
            .elseif LC_SPEED = 1
                .byte diskio::TABOO_LEVELCRUSH_SPEED_1
                .out "  decompression with Taboo Levelcrush at speed 1"
            .elseif LC_SPEED = 2
                .byte diskio::TABOO_LEVELCRUSH_SPEED_2
                .out "  decompression with Taboo Levelcrush at speed 2"
            .elseif LC_SPEED = 3
                .byte diskio::TABOO_LEVELCRUSH_SPEED_3
                .out "  decompression with Taboo Levelcrush at speed 3"
            .elseif LC_SPEED = 4
                .byte diskio::TABOO_LEVELCRUSH_SPEED_4
                .out "  decompression with Taboo Levelcrush at speed 4"
            .elseif LC_SPEED = 5
                .byte diskio::TABOO_LEVELCRUSH_SPEED_5
                .out "  decompression with Taboo Levelcrush at speed 5"
            .elseif LC_SPEED = 6
                .byte diskio::TABOO_LEVELCRUSH_SPEED_6
                .out "  decompression with Taboo Levelcrush at speed 6"
            .endif
        .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER
            .byte diskio::BYTEBOOZER
            .out "  decompression with ByteBoozer"
        .elseif DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            .byte diskio::PUCRUNCH
            .out "  decompression with PuCrunch"
        .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
            .byte diskio::EXOMIZER
            .out "  decompression with Exomizer"
        .endif
    .endmacro

    .macro COMPRESSION_RESIFEATS
        .ifdef DYNLINKLDR
            .ifdef USER_COMPRESSION_RESIFEATS
                .if USER_COMPRESSION_RESIFEATS <> 0
                    USER_COMPRESSION_RESIFEATS_IMPL
                .else
                    DEFAULT_COMPRESSION_RESIFEATS
                .endif
            .else
                DEFAULT_COMPRESSION_RESIFEATS
            .endif
        .else
            DEFAULT_COMPRESSION_RESIFEATS
        .endif
    .endmacro
.endif

;------------------------------------------------------------------------------

.macro DEFAULT_LOAD_UNDER_IO_RAM_RESIFEATS
    .if LOAD_UNDER_D000_DFFF
        .byte diskio::UNDER_IO_RAM, diskio::D000_DFFF_SETS_RAM_CONFIG
        .out "  has to be able to load to $d000..$dfff"
    .else
        .out "  does not have to be able to load to $d000..$dfff"
    .endif
.endmacro

.macro LOAD_UNDER_IO_RAM_RESIFEATS
    .ifdef DYNLINKLDR
        .ifdef USER_LOAD_UNDER_IO_RAM_RESIFEATS
            .if USER_LOAD_UNDER_IO_RAM_RESIFEATS <> 0
                USER_LOAD_UNDER_IO_RAM_RESIFEATS_IMPL
            .else
                DEFAULT_LOAD_UNDER_IO_RAM_RESIFEATS
            .endif
        .else
            DEFAULT_LOAD_UNDER_IO_RAM_RESIFEATS
        .endif
    .else
        .if LOAD_UNDER_D000_DFFF
            .byte diskio::UNDER_IO_RAM, diskio::D000_DFFF_SETS_RAM_CONFIG
            .out "  loads to $d000..$dfff"
        .else
            .out "  no loading to $d000..$dfff"
        .endif
    .endif
.endmacro

;------------------------------------------------------------------------------

.macro DEFAULT_NON_BLOCKING_RESIFEATS
    .out "  is blocking"
.endmacro

.macro NON_BLOCKING_RESIFEATS
    .ifdef DYNLINKLDR
        .ifdef USER_NON_BLOCKING_RESIFEATS
            .if USER_NON_BLOCKING_RESIFEATS <> 0
                USER_NON_BLOCKING_RESIFEATS_IMPL
            .else
                DEFAULT_NON_BLOCKING_RESIFEATS
            .endif
        .else
            DEFAULT_NON_BLOCKING_RESIFEATS
        .endif
    .else
        .out "  is blocking"
    .endif
.endmacro

;------------------------------------------------------------------------------

.if NONBLOCKING_API
    .macro DEFAULT_NON_BLOCKING_RESIFEATS_NB
        .byte diskio::NON_BLOCKING, diskio::IS_NONBLOCKING
        .out "  is non-blocking"
    .endmacro

    .macro NON_BLOCKING_RESIFEATS_NB
        .ifdef DYNLINKLDR
            .ifdef USER_NON_BLOCKING_RESIFEATS_NB
                .if USER_NON_BLOCKING_RESIFEATS_NB <> 0
                    USER_NON_BLOCKING_RESIFEATS_NB_IMPL
                .else
                    DEFAULT_NON_BLOCKING_RESIFEATS_NB
                .endif
            .else
                DEFAULT_NON_BLOCKING_RESIFEATS_NB
            .endif
        .else
            .byte diskio::NON_BLOCKING, diskio::IS_NONBLOCKING
            .out "  is non-blocking"
        .endif
    .endmacro
.endif

;------------------------------------------------------------------------------

.macro DEFAULT_MEMDECOMP_TO_RESIFEATS
    .if MEM_DECOMP_TO_API
        .byte diskio::DESTINATION_OVERRIDE, diskio::DEST_OVERRIDE
        .out "  with destination override"
    .else
        .out "  without destination override"
    .endif
.endmacro

.macro MEMDECOMP_TO_RESIFEATS
    .ifdef DYNLINKLDR
        .ifdef USER_MEMDECOMP_TO_RESIFEATS
            .if USER_MEMDECOMP_TO_RESIFEATS <> 0
                USER_MEMDECOMP_TO_RESIFEATS_IMPL
            .else
                DEFAULT_MEMDECOMP_TO_RESIFEATS
            .endif
        .else
            DEFAULT_MEMDECOMP_TO_RESIFEATS
        .endif
    .else
        .if MEM_DECOMP_TO_API
            .byte diskio::DESTINATION_OVERRIDE, diskio::DEST_OVERRIDE
            .out "  with destination override"
        .else
            .out "  without destination override"
        .endif
    .endif
.endmacro

;------------------------------------------------------------------------------

.macro DEFAULT_CHAINED_COMPD_FILES_RESIFEATS
    .if CHAINED_COMPD_FILES
        .byte diskio::CHAINED_FILES, diskio::CHAINED_FILES_POSSIBLE
        .out "  must allow for chained files"
    .else
        .out "  does not need to allow for chained files"
    .endif
.endmacro

.macro CHAINED_COMPD_FILES_RESIFEATS
    .ifdef DYNLINKLDR
        .ifdef USER_CHAINED_COMPD_FILES_RESIFEATS
            .if USER_CHAINED_COMPD_FILES_RESIFEATS <> 0
                USER_CHAINED_COMPD_FILES_RESIFEATS_IMPL
            .else
                DEFAULT_CHAINED_COMPD_FILES_RESIFEATS
            .endif
        .else
            DEFAULT_CHAINED_COMPD_FILES_RESIFEATS
        .endif
    .else
        .if CHAINED_COMPD_FILES
            .byte diskio::CHAINED_FILES, diskio::CHAINED_FILES_POSSIBLE
            .out "  allows for chained files"
        .else
            .out "  does not nallow for chained files"
        .endif
    .endif
.endmacro

;------------------------------------------------------------------------------

.macro DEFAULT_LOAD_UNCOMPD_ENDADDR_RESIFEATS
    .if END_ADDRESS_API
        .byte diskio::RETURNS_ENDADDR, diskio::ENDADDR_RETURNED
        .out "  must return end address"
    .else
        .out "  does not need to return end address"
    .endif
.endmacro

.macro LOAD_UNCOMPD_ENDADDR_RESIFEATS
    .ifdef DYNLINKLDR
        .ifdef USER_LOAD_UNCOMPD_ENDADDR_RESIFEATS
            .if USER_LOAD_UNCOMPD_ENDADDR_RESIFEATS <> 0
                USER_LOAD_UNCOMPD_ENDADDR_RESIFEATS_IMPL
            .else
                DEFAULT_LOAD_UNCOMPD_ENDADDR_RESIFEATS
            .endif
        .else
            DEFAULT_LOAD_UNCOMPD_ENDADDR_RESIFEATS
        .endif
    .else
        .if END_ADDRESS_API
            .byte diskio::RETURNS_ENDADDR, diskio::ENDADDR_RETURNED
            .out "  returns end address"
        .else
            .out "  does not return end address"
        .endif
    .endif
.endmacro

;------------------------------------------------------------------------------

.macro LOADFEATS_COMMON
    PLATFORM_FEATS
    SERIAL_BUS_FEATS
    SUPPORTED_DRIVES_FEATS
    SUPPORTED_DEVICE_NUMBERS_FEATS
    FORMAT_FEATS
    INTERRUPT_RESIFEATS
    FILE_FEATS
    LOAD_UNDER_IO_RAM_RESIFEATS
    REENTRANCE_FEATS
.endmacro


; generate jump table or module function table

.if LOAD_UNCOMPD_API

    .macro LOAD_UNCOMPRESSED_FEATURES
        .out " loaduncompd"
        LOADFEATS_COMMON
        LOAD_TO_FEATS
        LOAD_UNCOMPD_ENDADDR_RESIFEATS
        TIMER_FEATS
        PALNTSCACCEL_FEATS
        NON_BLOCKING_RESIFEATS
        .out "  loads unpacked"
        .out "  chained files not possible"
        .byte diskio::END_OF_LIST
    .endmacro

    .ifdef DYNLINKLDR
        .byte diskio::LOAD_UNCOMPRESSED
        LOAD_UNCOMPRESSED_FEATURES

        .define loaduncompd __LOADER_RUN__
        _A_ = 3
    .elseif .defined(DYNLINK)
        DYNLINKEXPORT diskio::LOAD_UNCOMPRESSED, loaduncompd
        LOAD_UNCOMPRESSED_FEATURES
    .else
        .export loaduncompd

        .if JUMP_TABLE
            .out " loaduncompd"
loaduncompd: jmp loaduncompd2
        .endif
    .endif
.else
    _A_ = 0
.endif

.if LOAD_COMPD_API

    .macro LOAD_COMPRESSED_FEATURES
        .out " loadcompd"
        LOADFEATS_COMMON
        COMPRESSION_RESIFEATS
        CHAINED_COMPD_FILES_RESIFEATS
        TIMER_FEATS
        PALNTSCACCEL_FEATS
        NON_BLOCKING_RESIFEATS
        .byte diskio::END_OF_LIST
    .endmacro

    .ifdef DYNLINKLDR
        .byte diskio::LOAD_COMPRESSED_EXO
        LOAD_COMPRESSED_FEATURES
        .define loadcompd __LOADER_RUN__+_A_
        _B_ = 3
    .elseif .defined(DYNLINK)
        DYNLINKEXPORT diskio::LOAD_COMPRESSED_EXO, loadcompd
        LOAD_COMPRESSED_FEATURES
    .else
        .export loadcompd

        .if JUMP_TABLE
            .out " loadcompd"
loadcompd:  jmp loadcompd2
        .endif
    .endif
.else
    _B_ = 0
.endif

.if MEM_DECOMP_API

    .macro MEMDECOMP_FEATS_COMMON
        ALL_PLATFORMS_FEATS
        COMPRESSION_RESIFEATS
        .ifdef DYNLINKLDR; PAL compatibility is usually sufficient for applications
            .byte diskio::PAL, diskio::PAL_COMPATIBLE
            .out "  must be PAL compatible"
            .out "  may be NTSC compatible"
            .out "  may be acceleration compatible"
        .else
            .byte diskio::PAL, diskio::PAL_COMPATIBLE
            .out "  PAL compatible"
            .byte diskio::NTSC, diskio::NTSC_COMPATIBLE
            .out "  NTSC compatible"
            .byte diskio::ACCELERATION, diskio::ACCELERATION_COMPATIBLE
            .out "  acceleration compatible"
        .endif
        .byte diskio::UNDER_IO_RAM, diskio::D000_DFFF_POSSIBLE
        .out " decompression to $d000..$dfff when $01 is set accordingly"
        MEMDECOMP_TO_RESIFEATS
        INTERRUPT_RESIFEATS
        TIMER_FEATS
        REENTRANCE_FEATS
        NON_BLOCKING_RESIFEATS
    .endmacro

    .ifdef DYNLINKLDR
        .out " memdecomp"
        .byte diskio::MEM_DECOMPRESS_EXO
        MEMDECOMP_FEATS_COMMON
        .byte diskio::END_OF_LIST

        .define memdecomp __LOADER_RUN__+_A_+_B_
        _C_ = 3
    .elseif .defined(DYNLINK)
        .out " memdecomp"
        DYNLINKEXPORT diskio::MEM_DECOMPRESS_EXO, memdecomp
        MEMDECOMP_FEATS_COMMON
        .byte diskio::END_OF_LIST
    .else
        .export memdecomp

        .if JUMP_TABLE
            .out " memdecomp"
memdecomp:  jmp memdecomp2
        .endif
    .endif

    .if CHAINED_COMPD_FILES
        .ifdef DYNLINKLDR
            .out " cmemdecomp"
            .byte diskio::MEM_CDECOMPRESS_EXO
            MEMDECOMP_FEATS_COMMON
            CHAINED_COMPD_FILES_RESIFEATS
            .byte diskio::END_OF_LIST

            .define cmemdecomp __LOADER_RUN__+_A_+_B_+_C_
            _D_ = 3
        .elseif .defined(DYNLINK)
            .out " cmemdecomp"
            DYNLINKEXPORT diskio::MEM_CDECOMPRESS_EXO, cmemdecomp
            MEMDECOMP_FEATS_COMMON
            CHAINED_COMPD_FILES_RESIFEATS
        .byte diskio::END_OF_LIST
        .else
            .export cmemdecomp

            .if JUMP_TABLE
                .out " cmemdecomp"
cmemdecomp: jmp cmemdecomp2
            .endif
        .endif
    .else
        _D_ = 0
    .endif
.else
    _C_ = 0
    _D_ = 0
.endif

.if OPEN_FILE_POLL_BLOCK_API
    .macro OPEN_FILE_FEATURES
        .out " openfile"
        LOADFEATS_COMMON
        LOAD_TO_FEATS
        TIMER_FEATS
        PALNTSCACCEL_FEATS
        NON_BLOCKING_RESIFEATS
        .byte diskio::END_OF_LIST
    .endmacro

    .macro POLL_LOADING_FEATURES
        .out " pollblock"
        PLATFORM_FEATS
        LOAD_UNDER_IO_RAM_RESIFEATS
        LOAD_UNCOMPD_ENDADDR_RESIFEATS
        INTERRUPT_RESIFEATS
        TIMER_FEATS
        PALNTSCACCEL_FEATS
        REENTRANCE_FEATS
        NON_BLOCKING_RESIFEATS
        .byte diskio::END_OF_LIST
    .endmacro

    .ifdef DYNLINKLDR
        .byte diskio::OPEN_FILE
        OPEN_FILE_FEATURES
        .define openfile __LOADER_RUN__+_A_+_B_+_C_+_D_
        _E_ = 3

        .byte diskio::POLL_LOADING
        POLL_LOADING_FEATURES
        .define pollblock __LOADER_RUN__+_A_+_B_+_C_+_D_+_E_
        _F_ = 3

    .elseif .defined(DYNLINK)

        DYNLINKEXPORT diskio::OPEN_FILE, openfile
        OPEN_FILE_FEATURES

        DYNLINKEXPORT diskio::POLL_LOADING, pollblock
        POLL_LOADING_FEATURES
    .else
        .export openfile
        .export pollblock

        .if JUMP_TABLE && (!NONBLOCKING_API)
            .out " openfile"
    openfile:   jmp openfile2
            .out " pollblock"
    pollblock:  jmp pollblock2
        .endif
    .endif
.else
    _E_ = 0
    _F_ = 0
.endif

.if UNINSTALL_API

    .macro UNINSTALLFEATS_COMMON
        PLATFORM_FEATS
        INTERRUPT_RESIFEATS
        .ifdef DYNLINKLDR; PAL compatibility is usually sufficient for applications
            .byte diskio::PAL, diskio::PAL_COMPATIBLE
            .out "  must be PAL compatible"
            .out "  may be NTSC compatible"
            .out "  may be acceleration compatible"
        .else
            .byte diskio::PAL, diskio::PAL_COMPATIBLE
            .out "  PAL compatible"
            .byte diskio::NTSC, diskio::NTSC_COMPATIBLE
            .out "  NTSC compatible"
            .byte diskio::ACCELERATION, diskio::ACCELERATION_COMPATIBLE
            .out "  acceleration compatible"
        .endif
        REENTRANCE_FEATS
    .endmacro

    .macro UNINSTALL_FEATURES
        .out " uninstall"
        UNINSTALLFEATS_COMMON
        TIMER_FEATS
        NON_BLOCKING_RESIFEATS
        .byte diskio::END_OF_LIST
    .endmacro

    .ifdef DYNLINKLDR
        .if (!NONBLOCKING_API) | UNINSTALL_RUNS_DINSTALL; don't import the install function if the installnb function is requested as well
            .byte diskio::UNINSTALL
            UNINSTALL_FEATURES
            .define uninstall __LOADER_RUN__+_A_+_B_+_C_+_D_+_E_+_F_
            _G_ = 3
        .else
            _G_ = 0
        .endif
    .elseif .defined(DYNLINK)
        DYNLINKEXPORT diskio::UNINSTALL, uninstall
        UNINSTALL_FEATURES
    .else
        .export uninstall

        .if (JUMP_TABLE) && (!.defined(DYNLINK))
            .out " uninstall"
    uninstall:  jmp uninstall2
        .endif
    .endif
.else
    _G_ = 0
.endif

.if NONBLOCKING_API

    .macro LOAD_UNCOMPRESSED_FEATURES_NB
        .out " ploadfil"
        LOADFEATS_COMMON
        LOAD_TO_FEATS
        LOAD_UNCOMPD_ENDADDR_RESIFEATS
        TIMER_FEATS_NB
        PALNTSCACCEL_FEATS_NB
        NON_BLOCKING_RESIFEATS_NB
        .out "  loads unpacked"
        .out "  chained files not possible"
        .byte diskio::END_OF_LIST
    .endmacro

    .ifdef DYNLINKLDR
        .byte diskio::LOAD_FILE_UNCOMPD_NB
        LOAD_UNCOMPRESSED_FEATURES_NB
        .define ploadfil __LOADER_RUN__+_A_+_B_+_C_+_D_+_E_+_F_+_G_
        _H_ = 3
    .elseif .defined(DYNLINK)
        DYNLINKEXPORT diskio::LOAD_FILE_UNCOMPD_NB, ploadfil
        LOAD_UNCOMPRESSED_FEATURES_NB
    .else
        .export ploadfil
        .if (JUMP_TABLE) && (!.defined(DYNLINK))
            .out " ploadfil"
ploadfil:   jmp ploadfil2
        .endif
    .endif

    .if UNINSTALL_API

        .macro UNINSTALL_FEATURES_NB
            .out " uninstallnb"
            UNINSTALLFEATS_COMMON
            TIMER_FEATS_NB
            .byte diskio::NON_BLOCKING, diskio::IS_NONBLOCKING
            .out "  for non-blocking routines"
            .byte diskio::END_OF_LIST
        .endmacro

        .ifdef DYNLINKLDR
            .byte diskio::UNINSTALL_NB
            UNINSTALL_FEATURES_NB
            .define uninstallnb __LOADER_RUN__+_A_+_B_+_C_+_D_+_E_+_F_+_G_+_H_
            _I_ = 3
        .elseif .defined(DYNLINK)
            DYNLINKEXPORT diskio::UNINSTALL_NB, uninstallnb
            UNINSTALL_FEATURES_NB
        .else
            .export uninstallnb
            .if (JUMP_TABLE) && (!.defined(DYNLINK))
                .out " uninstallnb"
uninstallnb: jmp uninstallnb
            .endif
        .endif
    .else
        _I_ = 0
    .endif; UNINSTALL_API
.else
    _H_ = 0
    _I_ = 0
.endif; NONBLOCKING_API

.if .defined(DYNLINKLDR) || .defined(DYNLINK)
    .byte diskio::END_OF_LIST
    .ifdef DYNLINK
base:
    .endif

    .ifdef DYNLINKLDR
        RESIDENTJUMPTABLESIZE    = _A_+_B_+_C_+_D_+_E_+_F_+_G_+_H_+_I_
        NUMIMPORTEDRESIDENTFUNCS = RESIDENTJUMPTABLESIZE / 3
        .if JUMP_TABLE
        DYNLINKOVERHEAD          = (NUMIMPORTEDINSTALLFUNCS + NUMIMPORTEDRESIDENTFUNCS) * 2
        .else
        DYNLINKOVERHEAD          = RESIDENTJUMPTABLESIZE + (NUMIMPORTEDINSTALLFUNCS + NUMIMPORTEDRESIDENTFUNCS) * 2
        .endif

        .define loadstatus   .lobyte(__LOADERZP_RUN__ + diskio::LOADSTATUSOFFS)

        .define loadaddrl    .lobyte(__LOADERZP_RUN__ + diskio::LOADADDRLOFFS)
        .define loadaddrh    .lobyte(__LOADERZP_RUN__ + diskio::LOADADDRHOFFS)

        .define decdestl     .lobyte(__LOADERZP_RUN__ + diskio::DECDESTLOFFS)
        .define decdesth     .lobyte(__LOADERZP_RUN__ + diskio::DECDESTHOFFS)

        .define endaddrl     .lobyte(__LOADERZP_RUN__ + diskio::ENDADDRLOFFS)
        .define endaddrh     .lobyte(__LOADERZP_RUN__ + diskio::ENDADDRHOFFS)

        .define bytesloadedl .lobyte(__LOADERZP_RUN__ + diskio::BYTESLOADEDLOFFS)
        .define bytesloadedh .lobyte(__LOADERZP_RUN__ + diskio::BYTESLOADEDHOFFS)

        .define param4       .lobyte(__LOADERZP_RUN__ + diskio::PARAM4OFFS)
        .define param5       .lobyte(__LOADERZP_RUN__ + diskio::PARAM5OFFS)
    .endif
.endif
