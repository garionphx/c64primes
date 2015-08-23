
.ifndef _DISKIO_INC_
_DISKIO_INC_ = 1

.scope diskio

VERSION_MAJOR            = 0 ; preliminary release
VERSION_MINOR            = 0

FUNCTION_DEMANDED        = %00000000; ORed to the number,
FUNCTION_DESIRED         = %10000000; desired functions that cannot be imported will not make the import fail

; the first byte of a function request
; determines the type of operation

.enum                               ; parameters
    INSTALL                  =    0 ; installs drive-side routines on current drive or all known drives on the bus
                                    ; in:  x/y  - if LOAD_ONCE: t/s, index, or pointer to 0-terminated file name
                                    ; out: a    - status
                                    ;      x    - drive type
                                    ;      y    - zeropage address containing a pointer to a zero-terminated version string
                                    ;      c    - error

    OPEN_FILE                =    1 ; in:  a    - open mode (may be omitted if only read-only routines are available
                                    ;      x/y  - t/s, index, or pointer to 0-terminated file name
                                    ; out: a    - status
                                    ;      x    - file descriptor (may be omitted if MAX_OPEN = 1), -1 if failed
                                    ;      c    - error

    CLOSE_FILE               =    2 ; in:  a    - file descriptor (may be omitted if MAX_OPEN = 1)
                                    ; never fails

    SEEK                     =    3 ; in:  a    - file descriptor (may be omitted if MAX_OPEN = 1)
                                    ;      x    - offset mode
                                    ;      p4/5 - offset
                                    ; out: a    - status
                                    ;      x/y  - new position
                                    ;      c    - error

    READ_BYTE                =    4 ; in:  a    - file descriptor (may be omitted if MAX_OPEN = 1)
                                    ; out: a    - value
                                    ;      c    - error

    UNREAD_BYTE              =    5 ; in:  a    - file descriptor (may be omitted if MAX_OPEN = 1)
                                    ; out: a    - status
                                    ;      c    - error

    WRITE_BYTE               =    6 ; in:  a    - file descriptor (may be omitted if MAX_OPEN = 1)
                                    ;      x    - value
                                    ;      c    - error

    POLL_LOADING             =    7 ; in:  a    - file descriptor (may be omitted if MAX_OPEN = 1), -1 for all open files
                                    ; out: a    - status
                                    ;      c    - 1: eof

    RENAME_FILE              =    8 ; in:  x/y  - old name
                                    ;      p4/5 - new name
                                    ; out: a    - status
                                    ;      c    - error

    DELETE_FILE              =    9 ; in:  x/y  - name or file index
                                    ; out: a    - status
                                    ;      c    - error

    LOAD_FILE                =   10 ; in:  x/y  - t/s, index, or pointer to 0-terminated file name
                                    ; out: a    - status
                                    ;      c    - error

    LOAD_UNCOMPRESSED        =   11
    LOAD_COMPRESSED_LC       =   12
    LOAD_COMPRESSED_BB       =   13
    LOAD_COMPRESSED_PU       =   14
    LOAD_COMPRESSED_EXO      =   15
    MEM_DECOMPRESS_LC        =   16
    MEM_DECOMPRESS_BB        =   17
    MEM_DECOMPRESS_PU        =   18
    MEM_DECOMPRESS_EXO       =   19
    MEM_CDECOMPRESS_LC       =   20
    MEM_CDECOMPRESS_BB       =   21
    MEM_CDECOMPRESS_PU       =   22
    MEM_CDECOMPRESS_EXO      =   23

    INSTALL_NONBLOCK         =   55
    UNINSTALL_NB             =   56

    ; direct io routines

    GET_SECTOR_COUNT         =   23 ; out: a    - number of logical sectors

    GET_SECTOR_SIZE          =   24 ; out: x/y  - sector size

    READ_SECTOR              =   25 ; in:  a    - head
                                    ;      x    - track
                                    ;      y    - sector
                                    ;      p4/5 - buffer
                                    ; out: a    - status
                                    ;      c    - error

    WRITE_SECTOR             =   26 ; in:  a    - head
                                    ;      x    - track
                                    ;      y    - sector
                                    ;      p4/5 - buffer
                                    ; out: a    - status
                                    ;      c    - error

    WRITE_SECTOR_VERIFY      =   27 ; in:  a    - head
                                    ;      x    - track
                                    ;      y    - sector
                                    ;      p4/5 - buffer
                                    ; out: a    - status
                                    ;      c    - error

    SECTOR_TO_PHYS           =   28 ; in:  x/y - logical sector number
                                    ; out: a   - head
                                    ;      x   - track
                                    ;      y   - sector

    PHYS_TO_SECTOR           =   29 ; in:  a   - head
                                    ;      x   - track
                                    ;      y   - sector
                                    ; out: x/y - logical sector number


    SET_CURRENT_DRIVE        =   30 ; in:  a   - new current drive
                                    ; out: a   - status
                                    ;      c   - error

    GET_FREE_CAPACITY        =   31 ; out: a   - status
                                    ;      x/y - free capacity in bytes

    LOAD_FILE_UNCOMPD_NB     =   32 ; non-blocking, loads a file in the background

    EXECUTE_COMMAND          =   33 ; in:  x/y - pointer to 0-terminated command string
                                    ; out: a   - status
                                    ;      x/y - pointer to 0-terminated error string
                                    ;      c   - error

    UNINSTALL                =  126 ; never fails

    SUPPORT_CHECK            =  127 ; in:  a   - command identifier as defined by this enum
                                    ;      x   - feature identifier as defined below
                                    ;      y   - feature level as defined below
                                    ; out: a   - status
                                    ;      c   - error/supported

    END_OF_LIST              =   -1
.endenum

; 6. read chunk into user buffer
; 7. get chunk (buffer owned by loader, streaming), chunk is valid until next call
; 9. write n bytes
; 11. save file
; 12. save file with verify
; 12. memory decompress file
; 13. continued memory decompress file
; 16. run custom drive code
; 17. serial bus lock
; 18. serial bus unlock
; 21. format disk
; 22. open dir
; 23. get next file
; 24. close dir
; 25. set current dir
; stat functions


; function features/constraints

; positive numbers,
; exact match required

; supported platforms, values according to enum patforms, may occur multiple times
PLATFORM                     =    0

; supported drives, values according to enum drivetypes, may occur multiple times
SUPPORTED_DRIVE              =    1

; supported device numbers
SUPPORTED_DEVICE_NUMBER      =    2

; file/disk format, may occur multiple times
FORMAT                       =    3
 CBM_STANDARD               = 0     ; 35 tracks on 1541, etc.
 CBM_EXTENDED               = 1     ; 41 tracks
 CBM1571_TWOSIDED           = 2     ; 70 tracks
 CBM1571_TWOSIDED_EXTENDED  = 3     ; 82 tracks
 GEOS                       = 4
 META_DATA                  = 5     ; Krill
 FM_VIA_GCR                 = 6     ; Krill
 META_DATA_FM_VIA_GCR       = 7     ; Krill
 AR_WARP_25                 = 8

; file identification method
FILE_IDENTIFICATION          =    4
 NAME                       = 0
 TRACK_SECTOR               = 1
 NAME_HASH                  = 2     ; 16-bit hash value
 INDEX_IN_DIRECTORY         = 3
 INDEX_IN_FILE_DB           = 4     ; IFFL

; compression
COMPRESSION                  =    5 ; unspecified implies uncompressed
 OXYPACKER                  = 0
 TABOO_LEVELCRUSH_SPEED_0   = 1
 TABOO_LEVELCRUSH_SPEED_1   = 2
 TABOO_LEVELCRUSH_SPEED_2   = 3
 TABOO_LEVELCRUSH_SPEED_3   = 4
 TABOO_LEVELCRUSH_SPEED_4   = 5
 TABOO_LEVELCRUSH_SPEED_5   = 6
 TABOO_LEVELCRUSH_SPEED_6   = 7
 BYTEBOOZER                 = 8
 PUCRUNCH                   = 9
 EXOMIZER                   = 10


; negative numbers,
; for the routines to be imported, higher or equal values are accepted

; serial bus constraints
SERIAL_BUS                   =  128 ; not specified implies only one drive allowed on the bus
 MANY_DRIVES_ON_SERIAL_BUS  = 0

; pal/ntsc/acceleration, not specified implies no compatibility
PAL                          =  129
 PAL_COMPATIBLE             = 0
NTSC                         =  130
 NTSC_COMPATIBLE            = 0
ACCELERATION                 =  131
 ACCELERATION_COMPATIBLE    = 0

; cia timer irq/nmi usage, not specified implies uses the facility
DOESNT_USE_CIA1_TA_IRQ       =  132
 CIA1_TA_IRQ_NOT_USED       = 0
DOESNT_USE_CIA1_TB_IRQ       =  133
 CIA1_TB_IRQ_NOT_USED       = 0
DOESNT_USE_CIA2_TA_NMI       =  134
 CIA2_TA_NMI_NOT_USED       = 0
DOESNT_USE_CIA2_TB_NMI       =  135
 CIA2_TB_NMI_NOT_USED       = 0

; screen (bad line)/interrupt/sprites tolerance
INTERRUPT_CONSTRAINTS        =  136 ; not specified implies interrupts and screen (including sprites) are disabled, no nmis allowed
 SCREEN_REMAINS_ON          = 0     ; screen on, interrupts and sprites are disabled, no nmis allowed
 INTERRUPTS_POSSIBLE        = 1     ; screen on, interrupts may be delayed, no nmis allowed, no sprites in non-irq space
 SPRITES_POSSIBLE           = 2     ; screen on, interrupts may be delayed, no nmis allowed, no sprite limitations
 ARBITRARY_INTERRUPTS       = 3     ; screen on, no interrupt delays, nmis allowed, no sprite limitations

; blocking/non-blocking call
NON_BLOCKING                 =  137 ; not specified implies blocking, when specified for the install/uninstall routines, it means non-blocking disk
                                    ; access calls are useable after calling it - the install/uninstall routines may still be blocking
                                    ; if defined for install function, either the blocking install function or the non-blocking one must be called
 IS_NONBLOCKING             = 0

; maximum number of characters in a file name; requires FILE_IDENTIFICATION = NAME, must be specified
FILENAME_MAX                 =  138 ; requires FILE_IDENTIFICATION = NAME, if FILENAME_MAX < number of chars allowed in the directory,
                                    ; the extra chars of the file name on disk are ignored

; if a filename in the directory is longer than FILENAME_MAX, only match on the first characters
IGNORES_EXTRA_CHARS          =  139; requires FILE_IDENTIFICATION = NAME, not specified implies extra chars are not ignored
 DOES_IGNORE_EXTRA_CHARS    = 0

; how many first files in a directory may be accessed
FIRST_FILES_ACCESSIBLE       =  140; requires FILE_IDENTIFICATION = NAME, must be specified
 NO_FIRST_FILES_LIMITATION  = -1

; maximum number of files open at the same time
MAX_OPEN                     =  141

; wildcards ? and * allowed for file names
FILENAME_WILDCARDS           =  142 ; requires FILE_IDENTIFICATION = NAME, not specified implies wildcards not allowed
 WILDCARDS_ALLOWED          = 0


; chained files, not specified implies no chained file operation
CHAINED_FILES                =  143
 CHAINED_FILES_POSSIBLE     = 0

DESTINATION_OVERRIDE         =  144 ; load/depack/save to specified memory address, determined by c-flag on call
                                    ; address is put to DESTINATION_LO and DESTINATION_HI before calling
                                    ; not specified implies no destination override
                                    ; XXX TODO mod routines to comply
 DEST_OVERRIDE              = 0

; works with file/data at $d000..$dfff
UNDER_IO_RAM                 =  145 ; not specified implies not to $d000..$dfff
 D000_DFFF_POSSIBLE         = 0     ; $01 must be set accordingly
 D000_DFFF_SETS_RAM_CONFIG  = 1     ; $01 is set accordingly by the routine

RETURNS_ENDADDR              =  146 ; returns end address in ENDADDRLO/ENDADDRHI after calling
                                    ; when loading without decompression, returns raw/unpacked data end address
                                    ; when loading with decompression, returns decompressed data end address
 ENDADDR_RETURNED           = 0

REENTRANCE                   =  147 ; not specified implies not re-entrant and no guard
 NOT_REENTRANT_GUARDED      = 0     ; not re-entrant, but has guards
 IS_REENTRANT               = 1

SELECTIVE_UNINSTALL          =  148 ; when c is set when calling, the drive-side routines are only uninstalled on the current drive
 UNINSTALL_SELECTIVE        = 0

; XXX TODO kernal fallback
; XXX TODO load progress
; XXX TODO idle bus lock

.scope platforms
    COMMODORE_64             =    0
    COMMODORE_128            =    1
    COMMODORE_16             =    2
    COMMODORE_PLUS4          =    3
    COMMODORE_VIC20          =    4
.endscope; platforms

.scope drivetypes
    DRIVE_1541               =    0
    DRIVE_1541_C             =    1
    DRIVE_1541_II            =    2
    DRIVE_1570               =    3
    DRIVE_1571               =    4
    DRIVE_1581               =    5
    DRIVE_CMD_FD_2000        =    6
    DRIVE_CMD_FD_4000        =    7
    DRIVE_CMD_HD             =    8

    DRIVE_GENERIC            =   -3
    DEVICE_UNKNOWN           =   -2
    DEVICE_NONE              =   -1
.endscope; drivetypes

; File mode constants, must match the values in the C headers
.enum
    O_RDONLY = $01
    O_WRONLY = $02
    O_RDWR   = $03
    O_CREAT  = $10
    O_TRUNC  = $20
    O_APPEND = $40
    O_EXCL   = $80
.endenum

.enum
    SEEK_CUR = 0
    SEEK_END = 1
    SEEK_SET = 2
.endenum

.scope status
    OK                       =  $00

    ; modload return codes
    MLOAD_OK                 =  $00 ; Module load successful
    MLOAD_ERR_READ           =  $01 ; Read error
    MLOAD_ERR_HDR            =  $02 ; Header error
    MLOAD_ERR_OS             =  $03 ; Wrong OS
    MLOAD_ERR_FMT            =  $04 ; Data format error
    MLOAD_ERR_MEM            =  $05 ; Not enough memory

    DYNLINK_FALLBACK_USED    =  $06 ; not an error, dynamic linkage failed an the fallback routines are used

    CHANGING_TRACK           =  $07 ; not an error, used by the polling system to minimize idle time overhead when changing the track

    BUSY                     =  $40 ; not an error, used by non-blocking routines, bit 6 must be set
    OPEN                     =  $41
    EOF                      =  $42
    CLOSED                   =  $43

    WRONG_VERSION            = -$0b
    HEADER_ERROR             = -$0a
    READ_ONLY                = -$09
    VERIFY_FAILED            = -$08
    NOT_SUPPORTED            = -$07 ; function is not implemented or not all requested functions have been imported
    INVALID_PARAMETERS       = -$06
    DEVICE_NOT_PRESENT       = -$05
    DEVICE_INCOMPATIBLE      = -$04 ; if LOAD_VIA_KERNAL_FALLBACK != 0, don't regard this as an error
    TOO_MANY_DEVICES         = -$03 ; for ONE_DRIVE_ON_BUS, if LOAD_VIA_KERNAL_FALLBACK != 0, don't regard this as an error
    GENERIC_KERNAL_ERROR     = -$02 ; an error occured while loading without installed drive code via KERNAL fallback,
                                    ; check the x register for further information
    UNSPECIFIED_ERROR        = -$01
.endscope; status

LOADSTATUSOFFS               =    0 ; for non-blocking operation
PARAM4OFFS                   =    1
PARAM5OFFS                   =    2
LOADADDRLOFFS                =    3
LOADADDRHOFFS                =    4
DECDESTLOFFS                 =    5
DECDESTHOFFS                 =    6
ENDADDRLOFFS                 =    7
ENDADDRHOFFS                 =    8
BYTESLOADEDLOFFS             =    9
BYTESLOADEDHOFFS             =   10

.struct FILE
    status                    .byte
    destination               .word
    endaddress                .word ; points to the first byte after the last byte
.endstruct

.struct IMPORT
    INSTALL_BASE              .word ; where the install routines will go
    INSTALL_SIZE              .word ; before dynamic linkage, contains the allowed maximum size, after successful dynamic linkage, contains the actual size
    RESIDENT_BASE             .word ; where the resident routines will go
    RESIDENT_SIZE             .word ; before dynamic linkage, contains the allowed maximum size, after successful dynamic linkage, contains the actual size
.endstruct                          ; after this struct, a list of the requested install functions,
                                    ; and a list of the requested resident functions follows, in this order;
                                    ; the zeropage symbols will be relocated to __LOADERZP_RUN__

.define DISKIO_SONAME "Disk I/O Library"

.endscope; diskio

.endif