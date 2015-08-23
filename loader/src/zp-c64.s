; the following zeropage variables can be
; overwritten when the loader is idle

__NOIMPORTVARS = 1
.include "loader.inc"

.segment "EXTZP"; not used otherwise, segment is not optional
                ; in the o65 built-in ld65 config

.macro alloc_zpvar symbol
symbol:       .res 1
    .exportzp symbol
.endmacro

.macro alloc_zpvar_2 symbol
symbol:       .res 2
    .exportzp symbol
.endmacro

.macro alloc_decompvars
    DECOMPVARS:
    .exportzp DECOMPVARS

    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
              .res 3
    .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER
              .res 6
    .elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
              .res 2
    .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
              .res 5
    .endif
.endmacro

.ifdef DYNLINK
    ; dynamic linking
    ; allocate zeropage variables
    ; caring for the
    ; fixed dynlink variable offsets
    ; while minimizing memory overhead
    ; by filling the gaps as good as possible

    .segment "ZEROPAGE" : zeropage

    .macro alloc_next_zpvar
        .if HAS_DECOMPRESSOR
            .ifndef LOADYBUF
                alloc_zpvar LOADYBUF
                .exitmacro
            .endif
            .ifndef YPNTRBUF
                alloc_zpvar YPNTRBUF
                .exitmacro
            .endif
        .endif

        .if NONBLOCKING_API
            .ifndef PACCUBUF
                alloc_zpvar PACCUBUF
            .endif
        .endif

        .ifndef fixed_zpvars_end
            .res 1; dummy variable
            .exitmacro
        .endif

        ; the following definitions occur in alloc_next_zpvars_2
        ; but are included here, too, because they might not be
        ; included yet using alloc_next_zpvars_2
        .ifndef BLOCKDESTLO
            alloc_zpvar BLOCKDESTLO
            .exitmacro
        .endif
        .ifndef BLOCKINDEX
            alloc_zpvar BLOCKINDEX; this one must be there after BLOCKDESTLO
            .exitmacro
        .endif

        .if (!HAS_DECOMPRESSOR) & LOAD_VIA_KERNAL_FALLBACK
            .ifndef LOADDESTPTR
                alloc_zpvar_2 LOADDESTPTR
                .exitmacro
            .endif
        .endif
    .endmacro

    .macro alloc_next_zpvars_2
        .ifndef BLOCKDESTLO
            alloc_zpvar BLOCKDESTLO
            alloc_zpvar BLOCKINDEX; this one must be there after BLOCKDESTLO
            .exitmacro
        .endif

        .if (!HAS_DECOMPRESSOR) & LOAD_VIA_KERNAL_FALLBACK
            .ifndef LOADDESTPTR
                alloc_zpvar_2 LOADDESTPTR
                .exitmacro
            .endif
        .endif

        alloc_next_zpvar
        alloc_next_zpvar
    .endmacro

    alloc_zpvar loadstatus  ; LOADSTATUSOFFS   = 0

    alloc_zpvar param4      ; PARAM4OFFS       = 1
    alloc_zpvar param5      ; PARAM5OFFS       = 2

    alloc_zpvar loadaddrl   ; LOADADDRLOFFS    = 3
    alloc_zpvar loadaddrh   ; LOADADDRHOFFS    = 4

    .if HAS_DECOMPRESSOR
    alloc_zpvar decdestl    ; DECDESTLOFFS     = 5
    alloc_zpvar decdesth    ; DECDESTHOFFS     = 6
    .else
        .if END_ADDRESS_API & LOAD_PROGRESS_API
        alloc_next_zpvars_2
        .endif
    .endif

    .if END_ADDRESS_API
    alloc_zpvar endaddrl    ; ENDADDRLOFFS     = 7
    alloc_zpvar endaddrh    ; ENDADDRHOFFS     = 8
    .else
        .if LOAD_PROGRESS_API
            .if DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
            alloc_decompvars
            .else
            alloc_next_zpvars_2
            .endif
        .endif
    .endif

    ; bytesloadedl/h is not endaddr - loadaddr while loading
    ; because blocks may be fetched out of order
    .if LOAD_PROGRESS_API
    alloc_zpvar bytesloadedl; BYTESLOADEDLOFFS = 9
    alloc_zpvar bytesloadedh; BYTESLOADEDHOFFS = 10
    .endif

    fixed_zpvars_end:

    ; decompressor
    .ifndef DECOMPVARS
    alloc_decompvars
    .endif

    .if HAS_DECOMPRESSOR & LOAD_VIA_KERNAL_FALLBACK
    LOADDESTPTR = DECOMPVARS
    .exportzp LOADDESTPTR
    .endif

    .repeat 32, I
        alloc_next_zpvar
    .endrep

.else

    ; no dynamic linking
    ; allocate zeropage variables
    ; without caring for the
    ; fixed dynlink variable offsets

    .segment "LOADERZP" : zeropage

    loader_zp_first = *
    .export loader_zp_first

    alloc_zpvar loadstatus

    alloc_zpvar loadaddrl
    alloc_zpvar loadaddrh

    .if HAS_DECOMPRESSOR
    alloc_zpvar decdestl
    alloc_zpvar decdesth
    .endif

    .if END_ADDRESS_API
    alloc_zpvar endaddrl
    alloc_zpvar endaddrh
    .endif

    ; bytesloadedl/h is not endaddr - loadaddr while loading
    ; because blocks may be fetched out of order
    .if LOAD_PROGRESS_API
    alloc_zpvar bytesloadedl
    alloc_zpvar bytesloadedh
    .endif

    .if HAS_DECOMPRESSOR
    alloc_zpvar LOADYBUF
    alloc_zpvar YPNTRBUF
    .endif

    .if NONBLOCKING_API
    alloc_zpvar PACCUBUF
    .endif

    alloc_zpvar BLOCKDESTLO
    alloc_zpvar BLOCKINDEX; this one must be there after BLOCKDESTLO

    .if (!HAS_DECOMPRESSOR) & LOAD_VIA_KERNAL_FALLBACK
    alloc_zpvar_2 LOADDESTPTR
    .endif

    ; decompressor
    alloc_decompvars

    .if HAS_DECOMPRESSOR & LOAD_VIA_KERNAL_FALLBACK
    LOADDESTPTR = DECOMPVARS
    .exportzp LOADDESTPTR
    .endif

    loader_zp_last = * - 1
    .export loader_zp_last

    .assert BLOCKINDEX = BLOCKDESTLO + 1, error, "BLOCKINDEXT != BLOCKDESTLO + 1"

.endif
