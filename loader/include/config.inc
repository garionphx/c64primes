
; configuration
; set .defines to non-0 to enable the corresponding features

; BEWARE:
;   Please keep in mind that most of the following parameters change the code size.
; In particular, some settings also change code size for the loader's drive code
; (i.e. the protocol you choose and enabling the custom drive code API),
; but since a stock 1541/41-C/41-II/70/71 has only 2K of RAM, it might not be possible to
; run the loader with all the desired features enabled at the same time -
; try a different loader protocol or file system in this case.
;   If you need a loader of minimal resident size (in the C64's memory),
; set all parameters in the 'increase code size' section to 0,
; and those in the 'code size reduction' sections to non-0,
; set FILESYSTEM = FILESYSTEMS::TRACK_SECTOR,
; and set DECOMPRESSOR = DECOMPRESSORS::NONE

; parameters

; these change installer code

.define MIN_DEVICE_NO             08 ; these two settings define the device number range when scanning for devices in install.s,
.define MAX_DEVICE_NO             14 ; devices beyond this range will not be recognized or useable


; these change the drive-side code

FILESYSTEM                         = FILESYSTEMS::DIRECTORY_NAME

.define LOAD_ONCE                  0 ; opens a file on initialization, loads it whenever the loader
                                     ; is called, then uninstalls the loader automatically;
                                     ; requires FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME
                                     ; reduces code size

.define DIRTRACK                  18 ; for FILESYSTEM::DIRECTORY_NAME, FILESYSTEM = FILESYSTEMS::DIRECTORY_INDEX, and FILESYSTEM::IFFL
                                     ; on 1541/41-C/41-II/70/71, starting track and sector location of the directory
.define DIRSECTOR                 00 ; (i.e., the loader's directory can be relocated to hide it from the normal dir command);
                                     ; ineffective for LOAD_ONCE and when loading via KERNAL with LOAD_VIA_KERNAL_FALLBACK != 0
.define FILENAME_MAXLENGTH        16 ; maximum length of file name, if a directory is capable of holding longer names, extra characters are ignored

; track jump settings
.define MINSTPSP                 $18 ; min. r/w head stepping speed on 1541/41-C/41-II/70/71
.define MAXSTPSP                 $10 ; max. r/w head stepping speed on 1541/41-C/41-II/70/71
.define STEPRACC                 $1c ; r/w head stepping acceleration on 1541/41-C/41-II/70/71


; features, won't change code size but reduce some functionality

; changes drive-side code
.define UNINSTALL_RUNS_DINSTALL    0 ; this is a patch until the CUSTOM_DRIVE_CODE_API is implemented, only use it if you really know what you're doing.
                                     ; calling uninstall will run dinstall on the drive side, enabling you to upload own custom drive code
                                     ; to the .org of the loader, with the size of the loader's drive code
                                     ; the loader's 1541/41-C/41-II drive-side code to be installed begins at $10 (assuming LOAD_ONCE = 0) and
                                     ; ends at drivebusy41 (exclusive), the byte at $10 must equal .hibyte(drivebusy41-*+$0100-$01),
                                     ; $0108 in the drive needs to hold the start-up address of your code MINUS 1, also the stack pointer is $07
                                     ; when running your code, so change it if you need a larger stack;
                                     ; to upload and run your code to the drive, use the UPLOAD_DRIVECODE_1541 <address> macro after including
                                     ; src/loader.inc;
                                     ; on a 1570/71, the code begins at $4d (assuming LOAD_ONCE = 0) and ends at drivebusy71, the byte at $4d must equal
                                     ; .hibyte(drivebusy71-*+$0100-$01), and you have to use the UPLOAD_DRIVECODE_1571 <address> macro to upload your code
                                     ; requires UNINSTALL_API


; following settings are independent from the installed drive code, you may use several
; c64-side binaries with different features with the same installed drive code

; features, increase code size

.define JUMP_TABLE                 0 ; add a jump table to the beginning of the LOADER segment, useful when importing the binaries without symbols

.define LOAD_UNDER_D000_DFFF       0 ; enable loading (and decompression) to the RAM at $d000-$dfff, this slows down loading quite a bit.
                                     ; your irq handlers will need to change $01 to enable the io-ram at $d000..$dfff,
                                     ; so make sure your irq handlers restore the $01 status to the value as when they are called.
                                     ; your irq's must run via $fffe/ff, since the ROM is disabled when accessing the RAM at $d000-$dfff
                                     ; this is not needed when only memdecompressing to $d000..$dfff (simply set $01 to $30 and jsr memdecomp in that case)

.define LOAD_TO_API                0 ; when the carry flag is set on load, the loader will use the address set in __LOADERZP_RUN__ + LOADADDRLOFFS and
                                     ; __LOADERZP_RUN__ + LOADADDRLOFFS as loading address
                                     ; for uncompressed files and ignore the file's loading address;
                                     ; this feature affects the loading address of compressed data when loading using loadcompd,
                                     ; it does not affect the address of the decompressed data

.define END_ADDRESS_API            0 ; after loading, the file's end address (last file byte + 1) is stored in endaddrl and endaddrh;
                                     ; for loading compressed files using loadcompd, the end address of the compressed data is returned.
                                     ; (the file's loading address can always be found in __LOADERZP_RUN__ + LOADADDRLOFFS/__LOADERZP_RUN__ + LOADADDRHOFFS
                                     ; after loading)

.define LOAD_UNCOMPD_API           0 ; include the loaduncompd routine to load files without decompressing

.define LOAD_COMPD_API             1 ; include the loadcompd routine to load and depack compressed files on the fly
                                     ; requires DECOMPRESSOR != DECOMPRESSORS::NONE and DECOMPRESSOR != DECOMPRESSORS::STREAM

.define OPEN_FILE_POLL_BLOCK_API   0 ; include the openfile and pollblock calls for polled loading

.define MEM_DECOMP_API             0 ; include routines for memory decompression; that is, loading and decompression can be separated,
                                     ; these are memdecomp (first crunched subfile) and cmemdecomp (consecutive crunched subfiles,
                                     ; just skips loading address bytes at the current compressed address and ignores decompression address parameters);
                                     ; decompress to $d000..$dfff doesn't need to have LOAD_UNDER_D000_DFFF enabled, just enable 64K ram before jsr
                                     ; memdecomp,
                                     ; requires DECOMPRESSOR != DECOMPRESSORS::NONE and DECOMPRESSOR != DECOMPRESSORS::STREAM
                                     ; this option does not implicitly turn on the LOAD_UNCOMPD_API

.define MEM_DECOMP_TO_API          0 ; when carry is set on decompression, the decompressor will use the address set in __LOADERZP_RUN__ + DECDESTLOFFS
                                     ; and __LOADERZP_RUN__ + DECDESTHOFFS as decompression destination address and ignore the file's decompression address.
                                     ; requires MEM_DECOMP_API != 0

.define CHAINED_COMPD_FILES        0 ; if non-0, it is possible to load files that are several compressed files glued one after another
                                     ; (simply concatenated compressed files with load address each);
                                     ; with these files, the loading address of the separate sub-files must be rising;
                                     ; simply load these chained compressed files using the loadcompd call as usual,
                                     ; or load uncompressed and use memdecomp for first, then cmemdecomp for
                                     ; subsequent compressed files (cmemdecomp will simply skip the load address);
                                     ; take care that the memory hole between the last and second-to-last chunk, or more, may be trashed, since
                                     ; the uncompressed data may occupy these holes - this does not apply if NO_DECOMPLOAD_OPTIMIZATION != 0;
                                     ; requires DECOMPRESSOR != DECOMPRESSORS::NONE and DECOMPRESSOR != DECOMPRESSORS::STREAM

.define NONBLOCKING_API            0 ; include non-blocking calls, so it is possible to do stuff in the main routine while timer nmis are used
                                     ; to poll and download the blocks while the main loop just does its thing - the loader works in the nmi
                                     ; instead of the main loop)

.define LOAD_VIA_KERNAL_FALLBACK   0 ; loads via the KERNAL routines if the loader's installation was not successful
                                     ; (i.e., if the loader is not installed due to an incompatible drive (currently everything that is not
                                     ; a 1541, 1541-C, 1541-II, 1570, or 1571, i.e. 1581, CMD FD, CMD HD, IDE64, etc.),
                                     ; or true drive emulation being disabled,
                                     ; or more than one device on the serial bus with PROTOCOL::TWO_BITS_ATN);
                                     ; for the sake of compatibility, only disable this option if you really need the space;
                                     ; attention: KERNAL, BASIC, and possible cartridge ROMs are enabled, so irq handlers are not
                                     ; allowed in the ranges $8000..$bfff and $d000..$ffff;
                                     ; attention: KERNAL routines might execute a cli, so make sure you have valid irq vectors and handlers,
                                     ; or disable all irq sources (not via sei), also make sure you correctly handle the different
                                     ; irq conditions when called via KERNAL vector ($0314) vs. non-KERNAL vector ($fffe) - best have KERNAL and
                                     ; BASIC enabled before calling the loader, so you only need the KERNAL vector irq handler (please note that
                                     ; your handler code is delayed a little when called via $0314 rather than $fffe);
                                     ; furthermore, your irq handlers can be delayed for some rasterlines up to several frames
                                     ; (but that is unlikely for devices not using the serial bus);
                                     ; requires FILESYSTEM = FILESYSTEMS::DIRECTORY_NAME or FILESYSTEM = FILESYSTEMS::DIRECTORY_INDEX

.define LOAD_PROGRESS_API          0 ; keeps updating bytesloadedl/h while loading with the amount
                                     ; of bytes that have been loaded (not decompressed!) so far;
                                     ; to be used for progress displays

.define IDLE_BUS_LOCK              0 ; allow for arbitrary $dd00 writes ($00-$ff) when the loader
                                     ; is idle (good for raster routines with lda #val:sta $d018:sta $dd00, e.g.)
                                     ; when not using the TWO_BITS_ATN protocol, the writes must have a minimum distance of 15 µs
                                     ; (i.e., 15 cycles at 1 MHz); however, if one of these $dd00 writes enables the ATN OUT bit
                                     ; (bit 3 in $dd00), only one device on the bus is allowed for all protocols

.define UNINSTALL_API              0 ; include an uninstallation routine


; code size reduction to the cost of speed or functionality

; parameters

LC_SPEED                       =   6 ; Taboo Levelcrush speed, irrelevant when using other compressors

; non-blocking operation: these intervals are timed for 1541/1541-C/1541-II loading speed, unit is charlines (= 8 scanlines)
POLLINGINTERVAL_STARTLOAD      = $27 ; 1 PAL frame, spin-up and track seek
POLLINGINTERVAL_TRACKCHANGE    = $27 ; 1 PAL frame, track seek
POLLINGINTERVAL_GETBLOCK       = $16 ; time to load and decode the next block
POLLINGINTERVAL_BLOCKSOON      = $02 ; timer interval after one of the bigger intervals listed above
POLLINGINTERVAL_REPOLL         = $01 ; maximum polling frequency, block is ready but downloading
                                     ; is postponed because of a running irq handler

DECOMPRESSOR                   = DECOMPRESSORS::EXOMIZER


; INEFFECTIVE
; following assignments and defines are ineffective in this version

; features, decrease code size

; the following two options are combineable
; both options enabled at the same time accelerate the loader by ~33% on 1541/41-C/41-II, and ~17% on 1570/71, and 0% for other drives

; INEFFECTIVE
.define FAST_FILE_FORMAT           0 ; load all files encoded with the fast file format, this saves up to 1 revolution per track and loading is ~17% faster
                                     ; compared to normal encoding on a 1541/41-C/41-II/70/71 - this has no influence (i.e., only minimal negative influence)
                                     ; on the speed of loading on a 1581/fd/hd,
                                     ; this reduces a block's data capacity by 2 bytes,
                                     ; loading files encoded in standard format or fast encoding format only is not possible with this option enabled
                                     ; use cc1541 to copy files onto d64/d71 images in the fast file format

; INEFFECTIVE
.define FAST_ENCODING_FORMAT       0 ; load all files encoded with the fast encoding format, this saves 1 revolution per track and loading is ~17% faster
                                     ; compared to normal encoding on a 1541/41-C/41-II (not 1570/71!),
                                     ; this reduces a block's data capacity by 96 bytes,
                                     ; loading files encoded in standard format or fast file format only is not possible with this option enabled;
                                     ; use cc1541 to copy files onto d64/d71 images in the fast encoding format


; features, increase code size
; INEFFECTIVE
.define DECOMPLOAD_TO_API          0 ; when carry is set on load, the loader will use the address set in __LOADERZP_RUN__ + DECDESTLOFFS and
                                     ; __LOADERZP_RUN__ + DECDESTHOFFS as loading address for compressed files and ignore the decompressed address
                                     ; stored in the file

; INEFFECTIVE
.define STREAM_USEFRAMEDFORMAT     0 ; use the framed file format to prevent from buffer copying, the format is described in docs/Stream.txt
                                     ; requires DECOMPRESSOR = DECOMPRESSORS::STREAM

; INEFFECTIVE
.define CUSTOM_DRIVE_CODE_API      0 ; support routines for custom drive code that enables you to upload
                                     ; custom code to the drive and run it to use the drive's cpu as a co-processor;
                                     ; this won't work if the loader's drive code is not installed, with the loader being running in the
                                     ; KERNAL fallback mode

; code size reduction to the cost of speed

; INEFFECTIVE
.define NO_DECOMPLOAD_OPTIMIZATION 0 ; disables the decompression while loading speed optimization and stalls the drive until a whole block of data
                                     ; is decompressed;
                                     ; decreases combined loading+decompressing speed
                                     ; requires DECOMPRESSOR != DECOMPRESSORS::NONE and DECOMPRESSOR != DECOMPRESSORS::STREAM,
                                     ; and SEPARATE_DECOMP = 0

; parameters

; INEFFECTIVE
PROTOCOL = PROTOCOLS::TWO_BITS_ATN

; INEFFECTIVE
.define STREAM_BUFFERSIZE      $14d6 ; cyclic stream buffer size in bytes, see Linkfile for memory address,
                                     ; must be at least 254*21 = $14d6 bytes (i.e., it needs to hold at least one track worth of data)
                                     ; requires DECOMPRESSOR = DECOMPRESSORS::STREAM or DECOMPRESSOR = DECOMPRESSORS::EXOSTREAM

; end ineffective assignments and defines
