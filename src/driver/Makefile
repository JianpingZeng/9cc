CC=gcc
LIBDIR=../lib
CFLAGS=-c -Wall -std=c99 -I$(LIBDIR)
LDFLAGS=-L$(LIBDIR) -lcc
INCLUDES=$(wildcard *.h)
SOURCES=mcc.c

ifeq ($(platform), mac)
SPEC=mac.c posix.c
endif

ifeq ($(platform), linux)
SPEC=linux.c posix.c
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
