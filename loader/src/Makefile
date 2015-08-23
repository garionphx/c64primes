# gnu makefile

SOMAJOR     = 0
SOMINOR     = 0

ECHO        = echo
CAT         = cat
MV          = mv
RM          = rm -f
RMDIR       = rmdir --ignore-fail-on-non-empty

AS          = ca65
LD          = ld65
AR          = ar65
VICE        = x64
C1541       = c1541

PU	        = pucrunch
EXO         = exomizer

PERL        = perl
ZIP         = zip -9


INCDIR      = ../../shared/
LDRINC      = ../include

BUILDDIR    = ../build
INTERMDIR   = ../build/intermediate

LOADERDEPS  = $(BUILDDIR) $(INTERMDIR) loader.s pudecomp.s bbdecomp.s lcdecomp.s exodecomp.s $(LDRINC)/loader.inc option-check.inc common-features.inc loader-jumptable.inc install-jumptable.inc $(LDRINC)/config.inc $(LDRINC)/diskio.inc Makefile
INSTALLDEPS = $(BUILDDIR) $(INTERMDIR) install.s drivecode1541.s drivecode1571.s drivecode1581.s $(LDRINC)/loader.inc $(LDRINC)/config.inc option-check.inc common-features.inc loader-jumptable.inc install-jumptable.inc $(LDRINC)/diskio.inc Makefile


AS_FLAGS   += -t c64 -U -I $(INCDIR) -I $(LDRINC)
ifneq ($(EXTCONFIG),)
AS_FLAGS   += -D EXTCONFIG -I $(EXTCONFIG)
endif
ifneq ($(DEBUG),)
AS_FLAGS   += -g
endif


default: lib

all: lib prg

# use these targets to build loader.prg and install.prg
prg: $(BUILDDIR)/loader.zip
bin: prg
binary: bin

libs: lib so

lib: $(BUILDDIR)/loader.a
so: $(BUILDDIR)/loader.so
dll: so


# directory targets

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(INTERMDIR):
	mkdir $(INTERMDIR)


# object targets

$(INTERMDIR)/zp-c64.o: $(BUILDDIR) $(INTERMDIR) zp-c64.s $(LDRINC)/config.inc
	$(AS) $(AS_FLAGS) -o $@ zp-c64.s

$(INTERMDIR)/loader.o: $(LOADERDEPS)
	$(AS) $(AS_FLAGS) -o $@ loader.s

$(INTERMDIR)/install.o: $(INSTALLDEPS)
	$(AS) $(AS_FLAGS) --cpu 6502X -o $@ install.s
	# --cpu 6502X for illegal opcodes


# binary targets
 # drive-side binaries
.PHONY: $(INTERMDIR)/drivecode.link

$(INTERMDIR)/drivecode.link:
	@$(ECHO) -e "MEMORY \n{\n DRIVERAM: start = \$$00, size = \$$0800, file = \"../build/intermediate/drivecode1541.prg\";\n}\n\nSEGMENTS\n{\n CODE: load = DRIVERAM;\n}" > $@

$(INTERMDIR)/drivecode1541.o: drivecode1541.s
	$(AS) $(AS_FLAGS) -D STANDALONE=1 --cpu 6502X -o $@ $^
	# --cpu 6502X for illegal opcodes

$(INTERMDIR)/drivecode1541.prg: $(INTERMDIR)/drivecode1541.o $(INTERMDIR)/drivecode.link
	$(LD) -C $(INTERMDIR)/drivecode.link $(INTERMDIR)/drivecode1541.o
	$(RM) a.out

drivecode1541.prg: $(INTERMDIR)/drivecode1541.prg

 # computer-side binaries
.PHONY: $(INTERMDIR)/binary.link $(BUILDDIR)/loader.prg $(INTERMDIR)/install-nonreloc.o $(BUILDDIR)/install.prg $(INTERMDIR)/loader-nonreloc.o $(INTERMDIR)/loader.map (BUILDDIR)/loadersymbols.inc

$(INTERMDIR)/loader-nonreloc.o: $(LOADERDEPS)
ifeq ($(RESIDENT),)
	@$(ECHO) "Usage: $(MAKE) prg INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@exit -1
endif
	$(AS) $(AS_FLAGS) -D RESIADDR=0x$(RESIDENT) -o $@ loader.s

$(INTERMDIR)/install-nonreloc.o: $(LOADERDEPS)
ifeq ($(INSTALL),)
	@$(ECHO) "Usage: $(MAKE) prg INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@exit -1
endif
	$(AS) $(AS_FLAGS) -D INSTADDR=0x$(INSTALL) --cpu 6502X -o $@ install.s
	# --cpu 6502X for illegal opcodes

$(INTERMDIR)/binary.link:
ifeq ($(ZP),)
	@$(ECHO) "Usage: $(MAKE) prg INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@exit -1
endif
	$(PERL) -e 'printf "MEMORY\
	{\
	 ZPRAM: start = \$$$(ZP), size = \$$%x;\
	 INSTALLRAM: start = \$$%x, size = \$$%x, file = \"../build/install.prg\";\
	 LOADERRAM: start = \$$%x, size = \$$%x, file = \"../build/loader.prg\";\
	}\
	\
	SEGMENTS\
	{\
	 LOADERZP: load = ZPRAM, type = zp;\
	 LOADER: load = LOADERRAM;\
	 LOADERINSTALL: load = INSTALLRAM;\
	}", 0x0100 - 0x$(ZP), 0x$(INSTALL) - 2, 0x10002 - 0x$(INSTALL), 0x$(RESIDENT) - 2, 0x10002 - 0x$(RESIDENT)' > $@

$(BUILDDIR)/loader.prg $(BUILDDIR)/install.prg: $(INTERMDIR)/zp-c64.o $(INTERMDIR)/loader-nonreloc.o $(INTERMDIR)/install-nonreloc.o $(INTERMDIR)/binary.link
	$(LD) -C $(INTERMDIR)/binary.link -vm -m $(INTERMDIR)/loader.map $(INTERMDIR)/zp-c64.o $(INTERMDIR)/loader-nonreloc.o $(INTERMDIR)/install-nonreloc.o
	$(RM) a.out

$(INTERMDIR)/loader.map: $(BUILDDIR)/install.prg $(BUILDDIR)/install.prg

$(BUILDDIR)/loadersymbols.inc: $(INTERMDIR)/loader.map
	$(PERL) make-loadersymbolsinc.pl $^ > $@

$(BUILDDIR)/loader.zip: $(BUILDDIR)/loadersymbols.inc $(BUILDDIR)/loader.prg $(BUILDDIR)/install.prg
	cd $(BUILDDIR) && $(ZIP) $@ $(^F)


# dynlink targets

$(INTERMDIR)/zp-c64.o65.o: zp-c64.s $(LDRINC)/config.inc
	$(AS) $(AS_FLAGS) -D DYNLINK -o $@ zp-c64.s

$(INTERMDIR)/loader.o65.o: $(LOADERDEPS)
	$(AS) $(AS_FLAGS) -D DYNLINK -o $@ loader.s

$(INTERMDIR)/install.o65.o: $(INSTALLDEPS)
	$(AS) $(AS_FLAGS) -D DYNLINK --cpu 6502X -o $@ install.s
	# --cpu 6502X for illegal opcodes

$(INTERMDIR)/install.o65: $(INTERMDIR)/install.o65.o $(INTERMDIR)/zp-c64.o65.o
	$(LD) -o $@ -t module -vm -m $@.map $^

$(INTERMDIR)/loader.o65: $(INTERMDIR)/loader.o65.o $(INTERMDIR)/zp-c64.o65.o
	$(LD) -o $@ -t module -vm -m $@.map $^

$(BUILDDIR)/loader.so: $(INTERMDIR)/loader.so.uncompd Makefile
	$(EXO) raw -m 512 $(INTERMDIR)/loader.so.uncompd -o $(INTERMDIR)/loader.so.temp
	$(ECHO) -n -e "\xC4ISK \xC9/\xCF \xCCIBRARY\x00\x$(SOMAJOR)\x$(SOMINOR)" | $(CAT) - $(INTERMDIR)/loader.so.temp > $@

$(INTERMDIR)/loader.so.uncompd: $(INTERMDIR)/install.o65 $(INTERMDIR)/loader.o65
	$(CAT) $^ > $@

$(INTERMDIR)/dynlink.o: dynlink.s $(LDRINC)/diskio.inc $(BUILDDIR)/loader.so
	$(AS) $(AS_FLAGS) -I $(CC65_INC) -o $*.o $<

$(INTERMDIR)/exostreamdecr1.o: exostreamdecr1.s
	$(AS) $(AS_FLAGS) -o $*.o $?

$(INTERMDIR)/modload.o: $(CC65_SRC)/libsrc/common/modload.s
	$(AS) $(AS_FLAGS) -I $(CC65_INC)/../asminc -o $@ $(CC65_INC)/../libsrc/common/modload.s

$(INTERMDIR)/memset.o: $(CC65_SRC)/libsrc/common/memset.s
	$(AS) $(AS_FLAGS) -I $(CC65_INC)/../asminc -o $@ $(CC65_INC)/../libsrc/common/memset.s

$(INTERMDIR)/pushax.o: $(CC65_SRC)/libsrc/runtime/pushax.s
	$(AS) $(AS_FLAGS) -I $(CC65_INC)/../asminc -o $@ $(CC65_INC)/../libsrc/runtime/pushax.s

$(INTERMDIR)/decax1.o: $(CC65_SRC)/libsrc/runtime/decax1.s
	$(AS) $(AS_FLAGS) -I $(CC65_INC)/../asminc -o $@ $(CC65_INC)/../libsrc/runtime/decax1.s

$(INTERMDIR)/incsp2.o: $(CC65_SRC)/libsrc/runtime/incsp2.s
	$(AS) $(AS_FLAGS) -I $(CC65_INC)/../asminc -o $@ $(CC65_INC)/../libsrc/runtime/incsp2.s


# loader.a library target

$(BUILDDIR)/loader.a: $(INTERMDIR)/zp-c64.o $(INTERMDIR)/loader.o $(INTERMDIR)/install.o $(INTERMDIR)/dynlink.o $(INTERMDIR)/exostreamdecr1.o $(INTERMDIR)/modload.o $(INTERMDIR)/memset.o $(INTERMDIR)/pushax.o $(INTERMDIR)/decax1.o $(INTERMDIR)/incsp2.o
	$(AR) a $@ $^


# debug targets

$(INTERMDIR)/debug.o: debug.s
	$(AS) $(AS_FLAGS) -o $@ $?

.PHONY: $(INTERMDIR)/debug.link

$(INTERMDIR)/debug.link:
ifeq ($(INSTALL),)
	@$(ECHO) "Usage: $(MAKE) prg INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@exit -1
endif
ifeq ($(RESIDENT),)
	@$(ECHO) "Usage: $(MAKE) prg INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@exit -1
endif
ifeq ($(ZP),)
	@$(ECHO) "Usage: $(MAKE) prg INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@exit -1
endif
	$(PERL) -e 'printf "MEMORY\
	{\
	 ZPRAM: start = \$$$(ZP), size = \$$%x;\
	 RAM:  start = \$$0800, size = \$$f800;\
	}\
	\
	SEGMENTS\
	{\
	 CODE: load = RAM, start = \$$%x;\
	 LOADERZP: load = ZPRAM, type = zp;\
	 LOADERINSTALL: load = RAM, start = \$$%x;\
	 LOADER: load = RAM, start = \$$%x;\
	}", 0x0100 - 0x$(ZP), 0x$(MAIN), 0x$(INSTALL), 0x$(RESIDENT)' > $@

$(INTERMDIR)/debug_uncompressed.prg: $(INTERMDIR)/debug.o $(BUILDDIR)/loader.a $(INTERMDIR)/debug.link
ifeq ($(IMAGE),)
	@$(ECHO) "Usage: $(MAKE) prg IMAGE=<image path> MAIN=<main address> INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@exit -1
endif
ifeq ($(MAIN),)
	@$(ECHO) "Usage: $(MAKE) prg IMAGE=<image path> MAIN=<main address> INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@exit -1
endif
	$(LD) -C $(INTERMDIR)/debug.link -vm -m $@.map -Ln $@.lbl -o $@ $(INTERMDIR)/debug.o $(BUILDDIR)/loader.a

$(INTERMDIR)/debug.prg: $(INTERMDIR)/debug_uncompressed.prg
	$(PU) -d -l 0x0800 -x 0x$(MAIN) -g 0x37 -i 0 $? $@

debug_image: $(INTERMDIR)/debug.prg
	$(C1541) -attach $(IMAGE) -delete debug -write $? debug

debug debug41: debug_image
	$(VICE) -autostart "$(IMAGE):debug" -drive8type 1541 -drive9type none

debug42: debug_image
	$(VICE) -autostart "$(IMAGE):debug" -drive8type 1542 -drive9type none

debug70: debug_image
	$(VICE) -autostart "$(IMAGE):debug" -drive8type 1570 -drive9type none

debug71: debug_image
	$(VICE) -autostart "$(IMAGE):debug" -drive8type 1571 -drive9type none


clean:
	$(RM) \
	$(INTERMDIR)/drivecode.link $(INTERMDIR)/drivecode1541.o $(INTERMDIR)/drivecode1541.prg \
	$(INTERMDIR)/zp-c64.o $(INTERMDIR)/loader.o $(INTERMDIR)/install.o \
	$(INTERMDIR)/loader-nonreloc.o $(INTERMDIR)/install-nonreloc.o $(BUILDDIR)/loader.prg $(BUILDDIR)/install.prg $(INTERMDIR)/binary.link $(BUILDDIR)/loadersymbols.inc \
	$(INTERMDIR)/loader.map $(INTERMDIR)/loader.o65.map $(INTERMDIR)/install.o65.map \
	$(INTERMDIR)/zp-c64.o65.o $(INTERMDIR)/install.o65.o $(INTERMDIR)/loader.o65.o $(INTERMDIR)/install.o65 $(INTERMDIR)/loader.o65 \
	$(INTERMDIR)/loader.so.uncompd $(INTERMDIR)/loader.so.temp \
	$(INTERMDIR)/dynlink.o $(INTERMDIR)/exostreamdecr1.o  $(INTERMDIR)/modload.o $(INTERMDIR)/memset.o $(INTERMDIR)/pushax.o $(INTERMDIR)/decax1.o $(INTERMDIR)/incsp2.o \
	$(BUILDDIR)/loader.a $(BUILDDIR)/loader.so \
	$(INTERMDIR)/debug.link $(INTERMDIR)/debug_uncompressed.prg.map $(INTERMDIR)/debug_uncompressed.prg.lbl $(INTERMDIR)/debug.o $(INTERMDIR)/debug_uncompressed.prg $(INTERMDIR)/debug.prg
	$(RMDIR) $(INTERMDIR)
