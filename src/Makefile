
TARGETS=libcc cpp cc mcc

.PHONY: all $(TARGETS)

ifndef platform
platform=mac
endif

all: $(TARGETS)

libcc:
	@cd lib && make

cpp: libcc
	@cd cpp && make platform=$(platform)

cc: libcc
	@cd cc && make

mcc: libcc
	@cd driver && make platform=$(platform)

clean:
	@cd lib && make clean
	@cd cpp && make clean
	@cd cc && make clean
	@cd driver && make clean
