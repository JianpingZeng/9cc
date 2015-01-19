CC=gcc
CFLAGS=-c -Wall -std=c99
LDFLAGS=
INCLUDES=$(wildcard *.h)

ifeq ($(platform), mac)
SOURCES=mcc.c mac.c
endif

ifeq ($(platform), linux)
SOURCES=mcc.c linux.c
endif

ifeq ($(platform), windows)
SOURCES=mcc.c windows.c
endif

OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=mcc

ifdef SOURCES
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
