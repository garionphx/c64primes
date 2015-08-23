
.fileopt comment, "Loader dynamic link code portion"
.fileopt compiler, "CA65"
.fileopt author, "Gunnar Ruthenberg"

.include "cpu.inc"

.include "basic.inc"

.include "loader.inc"

.segment "LOADERZP" : zeropage

.export __ZP_START__ : absolute
__ZP_START__ = __LOADERZP_RUN__

.export regbank
.export sp
.export ptr1
.export ptr3
.export tmp1
.export mode

regbank:    .res 6
sp:         .res 2
ptr1:       .res 2
ptr2:       .res 2
ptr3:       .res 2
ptr4:       .res 2
tmp1:       .res 2
tmp2:       .res 2
tmp3:       .res 1
mode:       .res 1

readdest:   .res 2

Module    = regbank + 0
TPtr      = regbank + 4


.segment "CODE"; the import routines are not in an extra segment because the linked funtions coming from the ca65 asm library
               ; are defined to run in the CODE segment, and because there is no rename function for segments in ld65

.include "kernal.inc"

.include "../asminc/modload.inc"
.include "diskio.inc"

.export importldr
.export _malloc
.export _free

.export get_crunched_byte
.export buffer_start_hi : absolute
.export buffer_len_hi   : absolute

.macpack longbranch


DIRFILENO         = $01
LIBFILENO         = $01

BUFFERSIZE        = $0200

FUNCTION_IMPORTED = $80


importldr:  sta modctrl + MOD_CTRL::CALLERDATA + 0
            sta sp + 0
            stx modctrl + MOD_CTRL::CALLERDATA + 1
            stx sp + 1

            lda #.lobyte(readfunc)
            sta modctrl + MOD_CTRL::READ + 0
            lda #.hibyte(readfunc)
            sta modctrl + MOD_CTRL::READ + 1

            ; find out how many requested functions there are and
            ; prepare check list to mark imported functions,
            ; it is stored backwards at the end of the install memory block            

            clc
            ldy #diskio::IMPORT::INSTALL_BASE + 0
            lda (sp),y
            ldy #diskio::IMPORT::INSTALL_SIZE + 0
            adc (sp),y
            sta ptr2 + 0
            ldy #diskio::IMPORT::INSTALL_BASE + 1
            lda (sp),y
            ldy #diskio::IMPORT::INSTALL_SIZE + 1
            adc (sp),y
            sta ptr2 + 1

            ldx #$00
            stx featsmatch
            stx numrqfuncs
            stx numrqinstf
            stx numrqresif
            ldy #.sizeof(diskio::IMPORT) - 1
countinstf: iny
            bne :+
            inx
            inc sp + 1
:           lda #diskio::END_OF_LIST
            cmp (sp),y
            beq countresif

            inc numrqinstf
            inc featsmatch
            inc numrqfuncs
            lda ptr2 + 0
            bne :+
            dec ptr2 + 1
:           dec ptr2 + 0
            txa
            pha
            tya
            ldx #$00
            sta (ptr2,x); store offset lo into the table
            pla
            tax
            lda ptr2 + 0
            bne :+
            dec ptr2 + 1
:           dec ptr2 + 0
            txa
            pha
            ldx #$00
            sta (ptr2,x); store offset hi into the table
            pla
            tax
            ; skip features
            lda #diskio::END_OF_LIST
:           iny
            bne :+
            inx
            inc sp + 1
:           cmp (sp),y
            bne :--
            beq countinstf

countresif: iny
            bne :+
            inx
            inc sp + 1
:           lda #diskio::END_OF_LIST
            cmp (sp),y
            beq endcountf

            inc numrqresif
            inc featsmatch
            inc numrqfuncs
            lda ptr2 + 0
            bne :+
            dec ptr2 + 1
:           dec ptr2 + 0
            txa
            pha
            tya
            ldx #$00
            sta (ptr2,x); store offset lo into the table
            pla
            tax
            lda ptr2 + 0
            bne :+
            dec ptr2 + 1
:           dec ptr2 + 0
            txa
            pha
            ldx #$00
            sta (ptr2,x); store offset hi into the table
            pla
            tax

            ; skip features
            lda #diskio::END_OF_LIST
:           iny
            bne :+
            inx
            inc sp + 1
:           cmp (sp),y
            bne :--
            jmp countresif

endcountf:  ; set the c stack pointer
            lda #.lobyte(__STACK_START__ + __STACK_SIZE__ - 1)
            sta sp + 0
            lda #.hibyte(__STACK_START__ + __STACK_SIZE__ - 1)
            sta sp + 1

            lda #$01
            sta fileindex
importloop: lda fileindex
            sta filecount

            ; the nice way is to open a file for the directory,
            ; then read a filename, try to import the corresponding file
            ; while leaving the directory file open, then continue with
            ; the next name in the dir if failed, etc.
            ; however, there seems to be a bug in the drive's firmwares
            ; when having 2 files open at the same time, so the dir file
            ; starts over at entry #7.
            ; so the awkward method of re-opening the directory after each
            ; failed import and then skipping the already processed files
            ; has to be used.

            lda #dirnameend - dirname
            ldx #.lobyte(dirname)
            ldy #.hibyte(dirname)
            jsr SETNAM
            lda #DIRFILENO
            ldx FA
            ldy #$00
            jsr SETLFS
            jsr OPEN
            bcc :+
            jsr READST
            tax
            lda #diskio::status::GENERIC_KERNAL_ERROR
            sec
            rts
            
:           ldx #DIRFILENO
            jsr CHKIN
            
            ; skip header
            ldx #$06
:           jsr CHRIN
            dex
            bne :-
:           jsr CHRIN
            tax
            bne :-
            
dirloop:    jsr CHRIN; BASIC line link lo
            jsr CHRIN; BASIC line link hi
            jsr CHRIN; file size lo
            jsr CHRIN; file size hi
:           jsr READST
            bne fallback
            jsr CHRIN
            cmp #'"'
            bne :-
            ldx #$00
            jsr CHRIN
:           sta name,x
            inx
            jsr CHRIN
            cmp #'"'
            beq :++
            cpx #nameend - name
            bne :-
:           jsr CHRIN
            cmp #'"'
            bne :-
:           txa
            dec filecount
            bne dirloop

            pha
            lda #DIRFILENO
            jsr CLOSE
            pla
            
            jsr importlibd

            inc fileindex
            bcs importloop

            ; library successfully imported
            lda #diskio::status::OK
            ldx version + 0
            ldy version + 1
           ;clc
            rts
            
            ; library import failed, so
            ; import the fallback library
fallback:   lda #$00
            sta STATUS

            lda IBASIN + $00
            pha
            lda IBASIN + $01
            pha
            lda #.lobyte(getmembyte)
            sta IBASIN + $00
            lda #.hibyte(getmembyte)
            sta IBASIN + $01
            
            lda #.lobyte(fallbackso)
            sta getmembyte + $01
            lda #.hibyte(fallbackso)
            sta getmembyte + $02
            jsr importlib

            sta tmp1 + 0                        
            pla
            sta IBASIN + $01
            pla
            sta IBASIN + $00
            lda tmp1 + 0
            bcs :+
            lda #diskio::status::DYNLINK_FALLBACK_USED
           ;clc
:           rts

:           inc getmembyte + $02
            rts
getmembyte: lda a:$00
            inc getmembyte + $01
            beq :-
            rts; one extra byte for one cycle less

importlibd: ldx #.lobyte(name)
            ldy #.hibyte(name)
            jsr SETNAM
            lda #LIBFILENO
            ldx FA
            ldy #$02
            jsr SETLFS
            jsr OPEN
            bcc :+
            jsr READST
            tax
            lda #diskio::status::GENERIC_KERNAL_ERROR
            sec
            rts
:           ldx #LIBFILENO
            jsr CHKIN

importlib:  tsx
            stx hwsp1

            ldx #$00
:           jsr getbyte
            cmp soname,x
            beq :+
            lda #diskio::status::HEADER_ERROR
            sec
            bcs errorret
:           cmp #$00
            beq :+
            inx
            bne :--
:
            jsr getbyte
            ldx #diskio::VERSION_MAJOR
            stx xret
            tay
            cmp #diskio::VERSION_MAJOR
            bne :+
            jsr getbyte
            cmp #diskio::VERSION_MINOR; smaller minor version numbers are ignored since the fallback library is newer
            bcs :++
:           lda #diskio::status::WRONG_VERSION
            sec
            bcs errorret
:
            jsr init_decruncher

            lda #diskio::IMPORT::INSTALL_SIZE
            sta mode
            jsr importsegment; import install functions
            stx xret
            cmp #diskio::status::OK + 1
            bcs errorret

            jsr get_decrunched_byte; skip load
            jsr get_decrunched_byte; address

            lda #diskio::IMPORT::RESIDENT_SIZE
            sta mode
            jsr importsegment; import resident functions
            stx xret

            cmp #diskio::status::OK + 1
            bcs errorret

            ; library import successful
            php
            pha

            ; store install and resident function block sizes
            lda modctrl + MOD_CTRL::CALLERDATA + 0
            sta sp + 0
            lda modctrl + MOD_CTRL::CALLERDATA + 1
            sta sp + 1
            ldy #diskio::IMPORT::INSTALL_SIZE + 0
            lda instsize + 0
            sta (sp),y
            iny
            lda instsize + 1
            sta (sp),y
            ldy #diskio::IMPORT::RESIDENT_SIZE + 0
            lda resisize + 0
            sta (sp),y
            iny
            lda resisize + 1
            sta (sp),y
            
            SKIPWORD
errorret:   php
            pha
            lda #LIBFILENO
            jsr CLOSE
            pla
            ldx xret
            plp
            rts

importsegment:
            lda #$00
            sta parsswitch

            tsx
            stx hwsp0

            lda #.lobyte(modctrl)
            ldx #.hibyte(modctrl)
            jmp _mod_load

_malloc:    sta ptr1 + 0
            stx ptr1 + 1
            
            inc parsswitch
            
            jsr get_decrunched_byte
            sta bytesparsd + 0
            jsr get_decrunched_byte
            sta bytesparsd + 1

            ldy #diskio::IMPORT::INSTALL_SIZE
            cpy mode
            bne :+
            jsr get_decrunched_byte
            sta version + 0
            jsr get_decrunched_byte
            sta version + 1
:
            ; the requested size is larger than needed
            ; subtract function table size
            sec
            lda ptr1 + 0; malloc size lo
            sbc bytesparsd + 0
            tay
            lda ptr1 + 1; malloc size hi
            sbc bytesparsd + 1
            tax
            tya
            
            ; add generated jump table size
            ldy #diskio::IMPORT::RESIDENT_SIZE - 1
            cpy mode
            ldy numrqinstf
            bcs :+
            ldy numrqresif
:           sty ptr1 + 0
            ldy #$03; 3 bytes per entry
:           clc
            adc ptr1 + 0
            bcc :+
            inx
:           dey
            bne :--
            
            ldy #diskio::IMPORT::INSTALL_SIZE
            cpy mode
            bne :+
            sta instsize + 0
            stx instsize + 1
            beq :++
:           sta resisize + 0
            stx resisize + 1
:
            ; for the installer, also add the
            ; space occupied by the functions import table
            ldy #diskio::IMPORT::INSTALL_SIZE
            cpy mode
            bne :++
            clc
            adc numrqfuncs
            bcc :+
            inx
            clc
:           adc numrqfuncs
            bcc :+
            inx
:
            ldy modctrl + MOD_CTRL::CALLERDATA + 0
            sty ptr1 + 0
            ldy modctrl + MOD_CTRL::CALLERDATA + 1
            sty ptr1 + 1
            ldy mode
            clc
            sbc (ptr1),y
            txa
            iny
            sbc (ptr1),y
            bcc :+
            ; not enough space
            lda #$00
            tax
            rts
:           ldy mode
            dey
            lda (ptr1),y
            tax
            dey
            lda (ptr1),y
            ; a/x contains address of allocated chunk
_free:      rts

readfunc:   sta readsize + 0
            stx readsize + 1

            tsx
            stx hwsp1
            
            jsr popax
            sta readdest + 0
            stx readdest + 1

            jsr popax; callerdata
            sta ptr1 + 0
            stx ptr1 + 1
            sta ptr4 + 0

            ldx readsize + 0
            lda readsize + 1
            pha
            lda parsswitch
            jeq noparse

            dec parsswitch

            ; read and parse the feature/jump table,
            ; compare with what is requested,
            ; and build the generated jump table

            ; calculate generated jump table size
            
            lda #diskio::IMPORT::RESIDENT_SIZE
            cmp mode
            beq :+; see which jump table's size to calculate

            ; install jump table
            ; jumptablesize = numrqinstf * 3
            lda numrqinstf
            asl
           ;clc
            adc numrqinstf
            bne :++

            ; resident jump table
            ; jumptablesize = numrqresif * 3
:           lda numrqresif
            sta featsmatch
            asl
           ;clc
            adc numrqresif

:           sta tmp1 + 0; generated jump table size
            
            lda #.hibyte($02); function table size word
            sta bytesparsd + 1
            lda #.lobyte($02)
            ldy #diskio::IMPORT::INSTALL_SIZE
            cpy mode
            bne :+
            lda #.lobyte($04)
:           sta bytesparsd + 0

            ; get the next present feature
parseloop:  jsr getlistbyt
            cmp #diskio::END_OF_LIST
            jeq endparse; the present function list in the library file
                        ; has only one end-of-list marker

            pha; function id
            jsr getlistbyt; .lobyte(offset)            
            sta ptr3 + 0
            jsr getlistbyt; .hibyte(offset)
            sta ptr3 + 1

            ; see if it is a requested function,
            ; check function type first
            pla; function id as given in the loader to be linked
            ldy numrqfuncs
            sty tmp1 + 1
findfunc:   ldy tmp1 + 1
            dey
            bpl :++
           
            ; no match, just disregard the function,
            ; it will merely senselessly occupy some memory
:           jsr getlistbyt; skip features
            cmp #diskio::END_OF_LIST
            bne :-          
            beq parseloop

:           pha
            sty tmp1 + 1; flag/offset table position
            tya
            asl
            tay
            clc
            lda (ptr2),y
            and #~FUNCTION_IMPORTED
            adc ptr1 + 1
            sta ptr4 + 1
            iny
            lda (ptr2),y; flag/offset table
            tay
            pla
            and #~diskio::FUNCTION_DESIRED
            cmp (ptr4),y; compare with requested function id
            beq funcfound                       
            ora #diskio::FUNCTION_DESIRED
            cmp (ptr4),y; compare with requested function id
            bne findfunc

            ; the function kind matches,
            ; so now, check features
funcfound:  
:           jsr getlistbyt; skip features
            cmp #diskio::END_OF_LIST
            bne :-

            ; the feature is requested, so mark it as found
            ; in the import flag/offset table and update its
            ; corresponding entry in the generated jump table

            clc
            lda numrqfuncs
            sbc tmp1 + 1; flag/offset table position
            ldy #diskio::IMPORT::RESIDENT_SIZE
            cpy mode
            bne :+
            sec
            sbc numrqinstf
:           sta tmp3 + 0; jump table entry

            ldy tmp1 + 1; flag/offset table position
            tya
            asl
            tay
            lda #FUNCTION_IMPORTED
            ora (ptr2),y; flag/offset table
            sta (ptr2),y; flag/offset table
 
            lda #-1
            cmp ptr3 + 0
            bne :+
            cmp ptr3 + 1
            beq nextfunc; offset of -1, so don't generate a jump table entry yet

:           lda tmp3 + 0; jump table entry
            asl
           ;clc
            adc tmp3 + 0
            tay            
            lda #OPC_JMP_ABS
            sta (readdest),y
            iny
            clc
            lda ptr3 + 0; offset lo
            adc tmp1 + 0; generated jump table size
            php
            clc
            adc readdest + 0; segment base lo
            sta (readdest),y
            iny
            lda ptr3 + 1; offset hi
            adc readdest + 1; segment base hi
            plp
            adc #$00
            sta (readdest),y
            
nextfunc:   dec featsmatch
            jmp parseloop

endparse:   lda featsmatch; is 0 when all requested functions have been imported
            jeq funcscompl

            ; not all functions were imported,
            ; so see if those not found were merely
            ; desired and not demanded
            ldy numrqfuncs; position in the imported functions flag/offset table
            lda #diskio::IMPORT::INSTALL_SIZE
            cmp mode
            beq chkdesired
            ldy numrqresif; position in the imported functions flag/offset table

chkdesired: sty tmp1 + 1; position in the imported functions flag table
            dey
            tya
            asl
            tay
            lda (ptr2),y; imported functions flag/offset table
            cmp #FUNCTION_IMPORTED
            bcs isimported; see if the function is already imported

            ; not, so now check if it was desired only
           ;and #~FUNCTION_IMPORTED
            adc ptr1 + 1
            sta ptr4 + 1
            iny
            lda (ptr2),y; imported functions flag/offset table
            tay
            lda (ptr4),y
            cmp #diskio::FUNCTION_DESIRED
            bcs :+
            ; if the function was demanded, this library import failed
            tax
            ldy #$ff
            jmp notsupport

:           ; otherwise, put lda #diskio::status::NOT_SUPPORTED : rts in place of the
            ; jmp in the jumptable

            lda tmp1 + 1; position in the imported functions flag table
            sec
            sbc #$01
            ldy #diskio::IMPORT::INSTALL_SIZE
            cpy mode
            beq :+
            ; resident routines
            cmp numrqresif
            bcs desirimprt; don't put the code if the install routines are just being imported
           ;clc
            adc numrqinstf
            jmp :++
            ; install routines
:           cmp numrqresif
            bcc desirimprt; don't put the code if the install routines are just being imported
:           eor #$ff
            clc
            adc numrqfuncs
            sta tmp3 + 0
            asl
            adc tmp3 + 0
            tay
            lda #OPC_LDA_IMM
            sta (readdest),y
            iny
            lda #diskio::status::NOT_SUPPORTED
            sta (readdest),y
            iny
            lda #OPC_RTS
            sta (readdest),y
            
desirimprt: dec featsmatch
            beq postdesird; all requested functions imported

isimported: ldy tmp1 + 1; position in the imported functions flag table
            dey
            bne chkdesired

postdesird: lda #diskio::IMPORT::INSTALL_SIZE
            cmp mode
            beq funcscompl            
            lda featsmatch
            beq funcscompl
            ; should not get here
            ldx #$ff
            ldy #$ff
            jmp notsupport

            ; all functions imported or just desired, so proceed
funcscompl: 
            ; subtract number of bytes occupied
            ; by magic number and present functions list;
            ; add size of generated jump table
            clc
            lda tmp1 + 0; generated jump table size
            adc Module + 0
            ldy Module + 1
            bcc :+
            iny
:           sec
            sbc bytesparsd + 0; magic word size and present functions list
            sta Module + 0
            tya
            sbc bytesparsd + 1
            sta Module + 1

            clc
            lda tmp1 + 0; generated jump table size
            adc TPtr + 0
            ldy TPtr + 1
            bcc :+
            iny
:           sec
            sbc bytesparsd + 0; magic word size and present functions list
            sta TPtr + 0
            tya
            sbc bytesparsd + 1
            tay
            sty TPtr + 1
            
            ldy #diskio::IMPORT::INSTALL_SIZE
            cpy mode; skip END_OF_LIST mark of the requested install functions list
            bne :+
            clc
            adc numrqfuncs; size of imported functins flag table
            sta TPtr + 0
            bcc :+
            inc TPtr + 1

:           ; for the bytes left to read,
            ; skip the memory area occupied
            ; by the generated jump table
            clc
            lda tmp1 + 0; generated jump table size
            adc readdest + 0
            sta readdest + 0
            bcc :+
            inc readdest + 1
:
            ldy #diskio::IMPORT::INSTALL_SIZE
            cpy mode
            bne :+
            clc
            lda readdest + 0
            adc version + 0
            sta version + 0
            lda readdest + 1
            adc version + 1
            sta version + 1
:
            ; from the number of bytes left to read,
            ; subtract number of bytes occupied
            ; by magic word and present features list
            sec
            txa
            sbc bytesparsd + 0
            tax
            pla
            sbc bytesparsd + 1
            pha
            
noparse:    txa
            bne :+
            pla
            sec
            sbc #$01
            pha
            SKIPWORD
readloop:   tya
            pha
            txa
:           pha
            jsr get_decrunched_byte
            ldy #$00
            sta (readdest),y
            pla
            tax
            pla
            tay

            inc readdest + 0
            bne :+
            inc readdest + 1
            
:           dex
            bne readloop
            dey
            cpy #-$01
            bne readloop

            lda readsize + 0
            ldx readsize + 1
            rts

notsupport: lda #diskio::status::NOT_SUPPORTED
            stx notsuppx
            ldx hwsp0
            txs
            ldx notsuppx
            sec
            rts
readerror:  ldx hwsp1
            txs
            lda #$00
            tax
            rts

getlistbyt: inc bytesparsd + 0
            bne :+
            inc bytesparsd + 1
:           stx xbuf
            jsr get_decrunched_byte
            ldx xbuf
            rts

get_crunched_byte:
getbyte:    php
            jsr READST
            bne readerror
            jsr CHRIN
            plp
            rts

.segment "RODATA"

dirname:    .byte '$'
dirnameend:
soname:     .byte DISKIO_SONAME
            .byte $00

fallbackso: .incbin "../build/loader.so"

.segment "BSS"

modctrl:    .tag MOD_CTRL

fileindex:  .res 1
filecount:  .res 1

hwsp0:      .res 1
hwsp1:      .res 1
notsuppx:   .res 1
xbuf:       .res 1
xret:       .res 1

parsswitch: .res 1
numrqinstf: .res 1
numrqresif: .res 1
numrqfuncs: .res 1
featsmatch: .res 1
readsize:   .res 2
bytesparsd: .res 2
instsize:   .res 2
resisize:   .res 2
version:    .res 2

name:       .res 16
nameend:

            .align 256
buffer_start_hi = .hibyte(*)
            .res BUFFERSIZE
buffer_len_hi = .hibyte(* - buffer_start_hi)
