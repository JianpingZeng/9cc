
EXECUTABLES=cpp cc mcc

.PHONY: all cpp cc mcc clean

ifndef platform
all:
	@echo "Usage: make platform=<mac, linux or windows>"
	@exit 1
else
all: $(EXECUTABLES)
endif

cpp:
	@cd cpp && make platform=$(platform)

cc:
	@cd cc && make

mcc:
	@cd driver && make platform=$(platform)

clean:
	@cd cpp && make clean
	@cd cc && make clean
	@cd driver && make clean
