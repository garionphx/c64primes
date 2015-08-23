# gnu makefile

ARCH        = $(shell uname | tr "[a-z]" "[A-Z]" | tr -c -d "[A-Z]")

MAKE        = make
CP          = cp
MV          = mv
DD          = dd
RM          = rm -f
RMDIR       = rmdir --ignore-fail-on-non-empty

AS          = ca65 -I ../src -I ../../shared
LD          = ld65
VICE        = x64
C1541       = c1541

LOADER      = ../src
LDRBIN      = ../build
LDRINC      = ../include
CC1541      = ../tools/cc1541

PU	        = pucrunch
BB          = BB

ifeq ($(ARCH),CYGWINNT)
LC          = ../tools/crush 6
else
LC          = wine ../tools/crush 6
endif

CONV        = java -cp ../tools CompressedFileConverter
EXO         = exomizer


BUILDDIR    = ../build
INTERMDIR   = ../build/intermediate


$(BUILDDIR):
	mkdir $(BUILDDIR)

$(INTERMDIR):
	mkdir $(INTERMDIR)


LOADERDEPS  = $(BUILDDIR) $(INTERMDIR) \
 $(LOADER)/loader.s $(LOADER)/zp-c64.s $(LOADER)/common-features.inc $(LOADER)/loader-jumptable.inc $(LOADER)/install-jumptable.inc $(LOADER)/pudecomp.s $(LOADER)/lcdecomp.s $(LOADER)/bbdecomp.s $(LOADER)/exodecomp.s \
 $(LOADER)/install.s $(LOADER)/drivecode1541.s $(LOADER)/drivecode1571.s $(LOADER)/drivecode1581.s \
 $(LOADER)/Makefile \
 $(LDRINC)/loader.inc $(LDRINC)/config.inc $(LDRINC)/diskio.inc

all: $(BUILDDIR)/loadertest.d64 $(BUILDDIR)/loadertest.d71 $(BUILDDIR)/loadertest.d81



$(LDRBIN)/loader.a: $(LOADERDEPS) $(LOADER)/dynlink.s $(LOADER)/exostreamdecr1.o $(LDRBIN)/loader.so
	$(MAKE) -C $(LOADER) DEBUG=1 lib

$(LDRBIN)/loader.so: $(LOADERDEPS)
	$(MAKE) -C $(LOADER) DEBUG=1 so

../version.inc: ../.svn/entries .svn/entries $(LOADER)/.svn/entries
	$(MAKE) -C .. version.inc

$(INTERMDIR)/test.o: test.s zp.inc $(LOADERDEPS) $(LOADER)/dynlink.s ../version.inc Makefile
	$(AS) -t c64 -I $(LDRINC) -I $(CC65_INC) --cpu 6502X -U -o $@ $<

$(INTERMDIR)/test_uncompressed.prg: Linkfile $(INTERMDIR)/test.o $(LDRBIN)/loader.a
	$(LD) -C Linkfile -vm -m $@.map -Ln $@.lbl -o $@ $(INTERMDIR)/test.o $(LDRBIN)/loader.a

$(INTERMDIR)/test.prg: $(INTERMDIR)/test_uncompressed.prg
	$(PU) -d -l 0x0800 -x 0x0800 -g 0x37 -i 0 $? $@


$(INTERMDIR)/sheba.lc: sheba.bin
	$(LC) $? sheba.lc
	$(MV) SHEBA.LC $@

$(INTERMDIR)/sheba.lcconv: sheba.bin $(INTERMDIR)/sheba.lc
	$(CONV) lc $^ $@

$(INTERMDIR)/sheba.bb: sheba.bin
	$(BB) $?
	$(MV) $?.bb $@

$(INTERMDIR)/sheba.bbconv: sheba.bin $(INTERMDIR)/sheba.bb
	$(CONV) bb $^ $@

$(INTERMDIR)/sheba.pu: sheba.bin
	$(PU) -c0 $? $@

$(INTERMDIR)/sheba.exo: sheba.bin
	$(EXO) mem -f $? -o $@


$(INTERMDIR)/prlogo.lc: prlogo.bin
	$(LC) $? prlogo.lc
	$(MV) PRLOGO.LC $@

$(INTERMDIR)/prlogo.lcconv: prlogo.bin $(INTERMDIR)/prlogo.lc
	$(CONV) lc $^ $@

$(INTERMDIR)/prlogo.bb: prlogo.bin
	$(BB) $?
	$(MV) $?.bb $@

$(INTERMDIR)/prlogo.bbconv: sheba.bin $(INTERMDIR)/prlogo.bb
	$(CONV) bb $^ $@

$(INTERMDIR)/prlogo.pu: prlogo.bin
	$(PU) -c0 $? $@

$(INTERMDIR)/prlogo.exo: prlogo.bin
	$(EXO) mem -f $? -o $@


# NAME T,S on the .d64 when using cc1541
# SB/SHEBA.BIN     $01,$00
# SP/SHEBA.PU      $02,$04
# SV/SHEBA.BBCONV  $03,$08
# SL/SHEBA.LCCONV  $03,$14
# SE/SHEBA.EXO     $04,$0b
# PB/PRLOGO.BIN    $04,$0f
# PP/PRLOGO.PU     $06,$13
# PV/PRLOGO.BBCONV $06,$04
# PL/PRLOGO.LCONV  $06,$0a
# PE/PRLOGO.EXO    $07,$10

$(BUILDDIR)/loadertest.d64: $(CC1541) $(INTERMDIR)/test.prg $(LDRBIN)/loader.so sheba.bin $(INTERMDIR)/sheba.pu $(INTERMDIR)/sheba.bbconv $(INTERMDIR)/sheba.lcconv $(INTERMDIR)/sheba.exo prlogo.bin $(INTERMDIR)/prlogo.pu $(INTERMDIR)/prlogo.bbconv $(INTERMDIR)/prlogo.lcconv $(INTERMDIR)/prlogo.exo
	$(CC1541) -n "NORMAL IS BORING" -i PLUSH -S 8 \
	 \
	 -f SB/SHEBA.BIN     -w sheba.bin\
	 -f SP/SHEBA.PU      -w $(INTERMDIR)/sheba.pu\
	 -f SV/SHEBA.BBCONV  -w $(INTERMDIR)/sheba.bbconv\
	 -f SL/SHEBA.LCCONV  -w $(INTERMDIR)/sheba.lcconv\
     -f SE/SHEBA.EXO     -w $(INTERMDIR)/sheba.exo\
	 \
   	 -f PB/PRLOGO.BIN    -w prlogo.bin\
	 -f PP/PRLOGO.PU     -w $(INTERMDIR)/prlogo.pu\
	 -f PV/PRLOGO.BBCONV -w $(INTERMDIR)/prlogo.bbconv\
	 -f PL/PRLOGO.LCCONV -w $(INTERMDIR)/prlogo.lcconv\
	 -f PE/PRLOGO.EXO    -w $(INTERMDIR)/prlogo.exo\
	 \
	 -f "LOADER.SO"   -w $(LDRBIN)/loader.so \
	 \
	 -f "LOADER TEST" -w $(INTERMDIR)/test.prg \
	$@

# NAME T,S on the .d71 when using c1541
# SB/SHEBA.BIN     $11,$00
# SP/SHEBA.PU      $13,$00
# SV/SHEBA.BBCONV  $13,$0b
# SL/SHEBA.LCCONV  $10,$0b
# SE/SHEBA.EXO     $14,$04
# PB/PRLOGO.BIN    $3a,$05
# PP/PRLOGO.PU     $3b,$12
# PV/PRLOGO.BBCONV $3c,$05
# PL/PRLOGO.LCCONV $3c,$0b
# PE/PRLOGO.EXO    $3c,$11

$(INTERMDIR)/filler.prg:
	$(DD) if=/dev/zero of=$@ bs=130048 count=1

$(BUILDDIR)/loadertest.d71: $(CC1541) $(CC1541) $(INTERMDIR)/test.prg $(LDRBIN)/loader.so sheba.bin $(INTERMDIR)/sheba.pu $(INTERMDIR)/sheba.bbconv $(INTERMDIR)/sheba.lcconv $(INTERMDIR)/sheba.exo $(INTERMDIR)/filler.prg prlogo.bin $(INTERMDIR)/prlogo.pu $(INTERMDIR)/prlogo.bbconv $(INTERMDIR)/prlogo.lcconv $(INTERMDIR)/prlogo.exo
	$(C1541) -format "normal is boring,+h" d71 $@
	$(C1541) -attach $@ \
	    -write sheba.bin  "sb/sheba.bin" \
	    -write $(INTERMDIR)/sheba.pu "sp/sheba.pu" \
	    -write $(INTERMDIR)/sheba.bbconv "sv/sheba.bbconv" \
	    -write $(INTERMDIR)/sheba.lcconv "sl/sheba.lcconv" \
	    -write $(INTERMDIR)/sheba.exo "se/sheba.exo" \
	    -write $(INTERMDIR)/filler.prg "filler0" \
	    -write $(INTERMDIR)/filler.prg "filler1" \
	    -write prlogo.bin "pb/prlogo.bin" \
	    -write $(INTERMDIR)/prlogo.pu "pp/prlogo.pu" \
	    -write $(INTERMDIR)/prlogo.bbconv "pv/prlogo.bbconv" \
	    -write $(INTERMDIR)/prlogo.lcconv "pl/prlogo.lcconv" \
	    -write $(INTERMDIR)/prlogo.exo "pe/prlogo.exo"\
	    -write $(LDRBIN)/loader.so "loader.so" \
	    -write $(INTERMDIR)/test.prg "loader test"

# NAME T,S on the .d81 when using c1541
# SB/SHEBA.BIN     $27,$00
# SP/SHEBA.PU      $27,$20
# SV/SHEBA.BBCONV  $29,$00
# SL/SHEBA.LCCONV  $29,$0c
# SE/SHEBA.EXO     $29,$18
# PB/PRLOGO.BIN    $29,$23
# PP/PRLOGO.PU     $26,$0f
# PV/PRLOGO.BBCONV $26,$03
# PL/PRLOGO.LCCONV $26,$09
# PE/PRLOGO.EXO    $26,$15

$(BUILDDIR)/loadertest.d81: $(CC1541) $(CC1541) $(INTERMDIR)/test.prg $(LDRBIN)/loader.so sheba.bin $(INTERMDIR)/sheba.pu $(INTERMDIR)/sheba.bbconv $(INTERMDIR)/sheba.lcconv $(INTERMDIR)/sheba.exo prlogo.bin $(INTERMDIR)/prlogo.pu $(INTERMDIR)/prlogo.bbconv $(INTERMDIR)/prlogo.lcconv $(INTERMDIR)/prlogo.exo
	$(C1541) -format "normal is boring,+h" d81 $@
	$(C1541) -attach $@ \
	    -write sheba.bin "sb/sheba.bin" \
	    -write $(INTERMDIR)/sheba.pu "sp/sheba.pu" \
	    -write $(INTERMDIR)/sheba.bbconv "sv/sheba.bbconv" \
	    -write $(INTERMDIR)/sheba.lcconv "sl/sheba.lcconv" \
	    -write $(INTERMDIR)/sheba.exo "se/sheba.exo" \
	    -write prlogo.bin "pb/prlogo.bin" \
	    -write $(INTERMDIR)/prlogo.pu "pp/prlogo.pu" \
	    -write $(INTERMDIR)/prlogo.bbconv "pv/prlogo.bbconv" \
	    -write $(INTERMDIR)/prlogo.lcconv "pl/prlogo.lcconv" \
	    -write $(INTERMDIR)/prlogo.exo "pe/prlogo.exo"\
	    -write $(LDRBIN)/loader.so "loader.so" \
	    -write $(INTERMDIR)/test.prg "loader test"

run run41: $(BUILDDIR)/loadertest.d64
	$(VICE) -autostart "$?:loader test" -drive8type 1541 -drive9type none

run41ii run42: $(BUILDDIR)/loadertest.d64
	$(VICE) -autostart "$?:loader test" -drive8type 1542 -drive9type none

run70: $(BUILDDIR)/loadertest.d64
	$(VICE) -autostart "$?:loader test" -drive8type 1570 -drive9type none

run71: $(BUILDDIR)/loadertest.d71
	$(VICE) -autostart "$?:loader test" -drive8type 1571 -drive9type none

run72: $(BUILDDIR)/loadertest.d64
	$(VICE) -autostart "$?:loader test" -drive8type 1571 -drive9type none

run81: $(BUILDDIR)/loadertest.d81
	$(VICE) -autostart "$?:loader test" -drive8type 1581 -drive9type none

run9 run941: $(BUILDDIR)/loadertest.d64
	$(VICE) -autostart "$?:loader test" -drive8type 1541 -drive9type 1541

run941ii run942: $(BUILDDIR)/loadertest.d64
	$(VICE) -autostart "$?:loader test" -drive8type 1541 -drive9type 1541

run970: $(BUILDDIR)/loadertest.d64
	$(VICE) -autostart "$?:loader test" -drive8type 1570 -drive9type 1541

run971: $(BUILDDIR)/loadertest.d71
	$(VICE) -autostart "$?:loader test" -drive8type 1571 -drive9type 1541

run972: $(BUILDDIR)/loadertest.d64
	$(VICE) -autostart "$?:loader test" -drive8type 1571 -drive9type 1541

run981: $(BUILDDIR)/loadertest.d81
	$(VICE) -autostart "$?:loader test" -drive8type 1581 -drive9type 1541


clean:
	$(RM) \
	$(INTERMDIR)/test.o $(INTERMDIR)/test_uncompressed.prg $(INTERMDIR)/test.prg \
	$(INTERMDIR)/test_uncompressed.prg.map $(INTERMDIR)/test_uncompressed.prg.lbl \
	$(INTERMDIR)/*.pu $(INTERMDIR)/*.bb $(INTERMDIR)/*.bbconv $(INTERMDIR)/*.lc $(INTERMDIR)/*.lcconv $(INTERMDIR)/*.exo \
	$(INTERMDIR)/filler.prg
	$(RMDIR) $(INTERMDIR)

distclean: clean
	$(MAKE) -C $(LOADER) clean
	$(MAKE) -C $(CC1541)_source clean

$(CC1541):
	$(MAKE) -C $(CC1541)_source
