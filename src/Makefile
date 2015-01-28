
TARGETS=libcc cpp cc mcc

.PHONY: all $(TARGETS)

ifndef platform
platform=mac
endif

ifndef BUILDDIR
BUILDDIR=../build
endif

all: $(TARGETS)

libcc:
	@cd lib && make BUILDDIR=$(BUILDDIR)

cpp: libcc
	@cd cpp && make platform=$(platform) BUILDDIR=$(BUILDDIR)

cc: libcc
	@cd cc && make BUILDDIR=$(BUILDDIR)

mcc: libcc
	@cd mcc && make platform=$(platform) BUILDDIR=$(BUILDDIR)

clean:
	@cd lib && make clean BUILDDIR=$(BUILDDIR) 
	@cd cpp && make clean BUILDDIR=$(BUILDDIR) platform=$(platform)
	@cd cc && make clean BUILDDIR=$(BUILDDIR)
	@cd mcc && make clean BUILDDIR=$(BUILDDIR) platform=$(platform)
