
; check configuration sensibility and define dynlink table

.include "common-features.inc"

;------------------------------------------------------------------------------

.macro DEFAULT_INTERRUPT_INSTFEATS
    ; when installing the drive routines, screen on is usually sufficient for applications
    .byte diskio::INTERRUPT_CONSTRAINTS, diskio::SCREEN_REMAINS_ON
    .out "  interrupts may be delayed, no sprites or nmis used"
.endmacro

.macro INTERRUPT_INSTFEATS
    .ifdef DYNLINKLDR
        .ifdef USER_INTERRUPT_INSTFEATS
            .if USER_INTERRUPT_INSTFEATS <> 0
                USER_INTERRUPT_INSTFEATS_IMPL
            .else
                DEFAULT_INTERRUPT_INSTFEATS
            .endif
        .else
            DEFAULT_INTERRUPT_INSTFEATS
        .endif
    .else
        .byte diskio::INTERRUPT_CONSTRAINTS, diskio::INTERRUPTS_POSSIBLE
        .out "  interrupts may be delayed, no nmis allowed, no sprites in non-irq space"
    .endif
.endmacro

;------------------------------------------------------------------------------

.macro DEFAULT_NON_BLOCKING_INSTFEATS
    .out "  does not enable non-blocking routines"
.endmacro

.macro NON_BLOCKING_INSTFEATS
    .ifdef DYNLINKLDR
        .ifdef USER_NON_BLOCKING_INSTFEATS
            .if USER_NON_BLOCKING_INSTFEATS <> 0
                USER_NON_BLOCKING_INSTFEATS_IMPL
            .else
                DEFAULT_NON_BLOCKING_INSTFEATS
            .endif
        .else
            DEFAULT_NON_BLOCKING_INSTFEATS
        .endif
    .else
        .out "  does not enable non-blocking routines"
    .endif
.endmacro

;------------------------------------------------------------------------------

.if NONBLOCKING_API
    .macro DEFAULT_NON_BLOCKING_INSTFEATS_NB
        .byte diskio::NON_BLOCKING, diskio::IS_NONBLOCKING
        .out "  enables non-blocking routines"
    .endmacro

    .macro NON_BLOCKING_INSTFEATS_NB
        .ifdef DYNLINKLDR
            .ifdef USER_NON_BLOCKING_INSTFEATS_NB
                .if USER_NON_BLOCKING_INSTFEATS_NB <> 0
                    USER_NON_BLOCKING_INSTFEATS_NB_IMPL
                .else
                    DEFAULT_NON_BLOCKING_INSTFEATS_NB
                .endif
            .else
                DEFAULT_NON_BLOCKING_INSTFEATS_NB
            .endif
        .else
            .byte diskio::NON_BLOCKING, diskio::IS_NONBLOCKING
            .out "  enables non-blocking routines"
        .endif
    .endmacro
.endif

;------------------------------------------------------------------------------

.macro INSTFEATS_COMMON
    PLATFORM_FEATS
    SERIAL_BUS_FEATS
    SUPPORTED_DRIVES_FEATS
    SUPPORTED_DEVICE_NUMBERS_FEATS
    FORMAT_FEATS
    INTERRUPT_INSTFEATS
    .if LOAD_ONCE
        FILE_FEATS
        LOAD_TO_FEATS
    .endif
    REENTRANCE_FEATS
.endmacro


.macro INSTALL_FEATURES
    .out " install"
    INSTFEATS_COMMON
    PALNTSCACCEL_FEATS
    TIMER_FEATS
    NON_BLOCKING_INSTFEATS
    .byte diskio::END_OF_LIST
.endmacro

.ifdef DYNLINKLDR
    .if (!NONBLOCKING_API) | UNINSTALL_RUNS_DINSTALL; don't import the install function if the installnb function is requested as well
        .byte diskio::INSTALL
        INSTALL_FEATURES
        .define install __LOADERINSTALL_RUN__
        _0_ = 3
    .else
        _0_ = 0
    .endif
.elseif .defined(DYNLINK)
    .byte diskio::INSTALL, .lobyte(install - base), .hibyte(install - base)
    INSTALL_FEATURES
.else
    .export install
    .if (JUMP_TABLE)
        .out "Jump table:"
        .out " install"
install:  jmp install2
    .endif
.endif


.if NONBLOCKING_API

    .macro NBINSTALL_FEATURES
        INSTFEATS_COMMON
        PALNTSCACCEL_FEATS_NB
        TIMER_FEATS_NB
        NON_BLOCKING_INSTFEATS_NB
        .byte diskio::END_OF_LIST
    .endmacro

    .ifdef DYNLINKLDR
        .out " installnb"
        .byte diskio::INSTALL_NONBLOCK
        NBINSTALL_FEATURES
        .define installnb __LOADERINSTALL_RUN__+_0_
        _1_ = 3
    .elseif .defined(DYNLINK)
        .out " installnb"
        .byte diskio::INSTALL_NONBLOCK, .lobyte(installnb - base), .hibyte(installnb - base)
        NBINSTALL_FEATURES
    .else
        .export installnb
        .if (JUMP_TABLE)
            .out " installnb"
installnb:  jmp installnb2
        .endif
    .endif

.else
    _1_ = 0
.endif

.ifdef DYNLINKLDR
    .byte diskio::END_OF_LIST

    INSTALLJUMPTABLESIZE    = _0_+_1_
    NUMIMPORTEDINSTALLFUNCS = INSTALLJUMPTABLESIZE / 3
.endif
