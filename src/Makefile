
TARGETS=cpp cc mcc

.PHONY: all $(TARGETS)

ifndef platform
platform=mac
endif

ifndef BUILDDIR
BUILDDIR=../build
endif

all: $(TARGETS)

cpp: 
	@cd cpp && make platform=$(platform) BUILDDIR=$(BUILDDIR)

cc: 
	@cd cc && make BUILDDIR=$(BUILDDIR)

mcc: 
	@cd mcc && make platform=$(platform) BUILDDIR=$(BUILDDIR)

clean:
	@cd cpp && make clean BUILDDIR=$(BUILDDIR) platform=$(platform)
	@cd cc && make clean BUILDDIR=$(BUILDDIR)
	@cd mcc && make clean BUILDDIR=$(BUILDDIR) platform=$(platform)
