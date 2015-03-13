
TARGETS=etc cc

.PHONY: all $(TARGETS)

ifndef platform
platform=mac
endif

ifndef BUILDDIR
BUILDDIR=../build
endif

all: $(TARGETS)

cc: 
	@cd cc && make BUILDDIR=$(BUILDDIR)

etc: 
	@cd etc && make platform=$(platform) BUILDDIR=$(BUILDDIR)

clean:
	@cd cc && make clean BUILDDIR=$(BUILDDIR)
	@cd etc && make clean BUILDDIR=$(BUILDDIR) platform=$(platform)
