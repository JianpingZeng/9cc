CC=gcc
CFLAGS=-c -Wall -std=c99
LDFLAGS=
INCLUDES=$(wildcard *.h)
SOURCES=mcc.c vector.c

ifeq ($(platform), mac)
SPEC=mac.c
endif

ifeq ($(platform), linux)
SPEC=linux.c
endif

ifeq ($(platform), windows)
SPEC=windows.c
endif

EXECUTABLE=mcc

ifdef SPEC
SOURCES+=$(SPEC)
OBJECTS=$(SOURCES:.c=.o)
all: $(SOURCES) $(EXECUTABLE)
else
all:
	@echo "Usage: make platform=<mac, linux or windows>"
	@exit 1
endif

$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

%.o: %.c $(INCLUDES)
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f *.o $(EXECUTABLE)
